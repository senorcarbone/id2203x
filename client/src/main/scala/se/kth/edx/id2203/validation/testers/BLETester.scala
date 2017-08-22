package se.kth.edx.id2203.validation.testers

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink.PerfectLinkInit
import se.kth.edx.id2203.core.ExercisePrimitives.{AddressUtils, PerfectP2PLink}
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.validation.ValidationUtils.{Property, SimValidator, ValidationGroup, ValidationSimulation}
import se.sics.kompics.network.{Address, Network}
import se.sics.kompics.simulator.network.identifier.Identifier
import se.sics.kompics.simulator.network.identifier.impl.SocketIdExtractor
import se.sics.kompics.simulator.network.impl.NetworkModels
import se.sics.kompics.simulator.network.impl.NetworkModels._
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.simulator.SimulationScenario
import se.sics.kompics.sl.{ComponentDefinition, Init, _}
import se.sics.kompics.sl.simulator.{Op, StartNode, _}
import se.sics.kompics.timer.Timer

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable._
import scala.concurrent.duration._
import scala.language.existentials
import scala.reflect.runtime.universe._
import scala.util.Random

object BLETester {

  val GLOBAL_STATE: String = "simulation.delivered"

  def apply[C <: ComponentDefinition : TypeTag](): collection.Iterable[ValidationGroup] = {
    val seed = 123;
    SimulationScenario.setSeed(seed);
    ValidationSimulation.runAndValidate(BLEValidation(asJavaClass[C](typeOf[C]), 15, 3, 100l, 50, 200).scenario);
  }


  class BLEInterceptor(init: Init[BLEInterceptor]) extends ComponentDefinition {

    val ble = requires(BallotLeaderElection);

    val self = init match {
      case Init(s: Address) => s
    };

    ble uponEvent {
      case x: BLE_Leader => handle {
        //        println(s"Process $self triggers $x")
        val gv = cfg.getValue[GlobalView]("simulation.globalview");
        val globalState = gv.getValue(BLETester.GLOBAL_STATE, classOf[HashMap[Address, ListBuffer[BLE_Leader]]]);
        val myList = globalState.get(self).getOrElse(ListBuffer.empty[BLE_Leader])
        myList.add(x);
        globalState.put(self, myList);
        gv.setValue(BLETester.GLOBAL_STATE, globalState);
      }
    }
  }

  class BLEParent(init: Init[BLEParent]) extends ComponentDefinition {

    val (addr, javaClass) = init match {
      case Init(s: Address, j: Class[_]) => (s, j.asInstanceOf[Class[ComponentDefinition]])
    };

    val network: PositivePort[Network] = requires[Network];
    val timer = requires[Timer];
    val pp2p = create(classOf[PerfectP2PLink], PerfectLinkInit(addr))
    val ble = create(javaClass, Init[ComponentDefinition](addr));

    val interceptor = create(classOf[BLEInterceptor], Init[BLEInterceptor](addr));

    connect[Network](network -> pp2p);
    connect(PerfectLink)(pp2p -> ble);
    connect[Timer](timer -> ble);
    connect(BallotLeaderElection)(ble -> interceptor);

  }

  /**
    * Ballot Leader Election
    *
    * Properties Checked
    * ------------------
    *
    * BLE1: Eventual Majority: Eventually a correct process should be considered a leader by a majority of processes.
    * BLE2: Ballot Monotonicity: Every new leader election should yield a monotonically increasing ballot number.
    *
    */
  case class BLEValidation(compClass: Class[_ <: ComponentDefinition], numNodes: Int, discNodes: Int, delay: Long, delta: Long, maxLinkDelay: Long) {

    import Distributions._

    assert(discNodes < numNodes)

    val topology = (1 to numNodes).map(AddressUtils.toAddress(_)).toList;
//    val deathOrder = getUniqueKillTargets(discNodes, numNodes);
    val sorted = topology.map(x => (x, x.hashCode())).sortBy(_._2).map(_._1);
    val failed = sorted.takeRight(discNodes)
    val correct = sorted.take(numNodes-discNodes)

    def getUniqueKillTargets(size: Int, max: Int): Array[Integer] = {
      val extRand = new Random();
      assert(size < max);
      var myUniqueSet = Set.empty[Integer];
      while (myUniqueSet.size != size) {
        myUniqueSet.add(Integer.valueOf(extRand.nextInt(max) + 1));
      }
      myUniqueSet.toArray;
    }
    

    def validateCompleteness(gv: GlobalView): Boolean = {
      val globalState = gv.getValue(BLETester.GLOBAL_STATE, classOf[mutable.HashMap[Address, ListBuffer[BLE_Leader]]]);
      correct.forall(z => {
        val leaders = globalState.get(z);

        if (!leaders.isEmpty) 
        {
          //check that the last reported leader is elected by a majority (or all...) of processes
          val elected = correct.map(z => globalState.get(z).get.last.leader).toSet
          if (elected.size != 1) {
            return false;
          }
          if(!correct.contains(elected.head)){
            return false;
          }
        }
        
        return true;
      });
    }
    
    def validateMonotonicity(gv: GlobalView): Boolean = {
      val globalState = gv.getValue(BLETester.GLOBAL_STATE, classOf[mutable.HashMap[Address, ListBuffer[BLE_Leader]]]);
      
      correct.forall(z => {
        globalState.get(z).get.foldLeft((0, true):(Long, Boolean))((b, next) => {
          b match {
            case (prev, false) => b;
            case (prev, true) =>
              if(prev < next.ballot) (next.ballot, true) else (next.ballot, false);
          }
        })._2
      })
    }

    val prop1: ValidationGroup = Property("Eventual Correctness")
      .addValidator(SimValidator(gv =>
        validateCompleteness(gv), 100000l,
        "Eventually a correct process should be considered a leader by a majority of processes",
        "There is no correct process that is eventually considered a leader by a majority of processes")).build()

    val prop2: ValidationGroup = Property("Ballot Monotonicity")
      .addValidator(SimValidator(gv => validateMonotonicity(gv), 100000l,
        "Every new leader election should yield a monotonically increasing ballot number",
        "The ballot number is not monotonically increasing among consecutive leader elections")).build();

    //Simulation Operations ----
    val setupOp = Op((_: Unit) => Setup((gv: GlobalView) => gv.setValue(BLETester.GLOBAL_STATE, mutable.HashMap.empty[Address, ListBuffer[BLE_Leader]])).build());

    val setUniformDelay = () => Op((maxDelay: java.lang.Long) =>
      ChangeNetwork(NetworkModels.withUniformRandomDelay(0, maxDelay, SimulationScenario.getRandom)));

    val delayOp = Op((_: Unit) => ChangeNetwork(withDisconnectedModel(
      new SocketIdExtractor(), NetworkModels.withUniformRandomDelay(5, maxLinkDelay, SimulationScenario.getRandom),
      new java.util.HashSet[Identifier](topology.slice(0, discNodes).map(x => new SocketIdExtractor().extract(x)).toSet[Identifier]))));

    val startBLEs = Op((self: Integer) => StartNode(AddressUtils.toAddress(self),
      Init[BLEParent](AddressUtils.toAddress(self), compClass),
      Map("ble.simulation.topology" -> topology, "ble.simulation.delay" -> delay, "ble.simulation.delta" -> delta)));

    val killOP = Op { (self: Integer) => KillNode(sorted(self.toInt)) }

    val scenario =
      ValidationSimulation(prop1, prop2) inParallel(
        raise(1, setupOp).arrival(Distributions.constant(0)), raise(1, setUniformDelay(), constant(maxLinkDelay)).arrival(constant(0)),
        raise(numNodes, startBLEs, 1.toN).arrival(constant(0))) andThen 10000.millis afterTermination
        raise(1, delayOp).arrival(constant(0)) andThen 500.millis afterTermination
        raise(1, setUniformDelay(), constant(0)).arrival(constant(0)) andThen 10000.millis afterTermination
        raise(discNodes, killOP, Distributions.intSeq(((numNodes-discNodes) until numNodes))).arrival(constant(0)) andThen 10000000l.millis afterTermination Terminate;
  }

}
