package se.kth.edx.id2203.validation.testers

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink.PerfectLinkInit
import se.kth.edx.id2203.core.ExercisePrimitives.{AddressUtils, PerfectP2PLink}
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.validation.ValidationUtils.{Property, SimValidator, ValidationGroup, ValidationSimulation}
import se.sics.kompics.KompicsEvent
import se.sics.kompics.network.{Address, Network}
import se.sics.kompics.simulator.adaptor.distributions.extra.GenIntSequentialDistribution
import se.sics.kompics.simulator.network.identifier.Identifier
import se.sics.kompics.simulator.network.identifier.impl.SocketIdExtractor
import se.sics.kompics.simulator.network.impl.NetworkModels
import se.sics.kompics.simulator.network.impl.NetworkModels._
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.simulator.SimulationScenario
import se.sics.kompics.sl.{ComponentDefinition, Init, _}
import se.sics.kompics.sl.simulator.{Op, StartNode, _}
import se.sics.kompics.timer.Timer

import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.collection.mutable._
import scala.concurrent.duration._
import scala.language.existentials
import scala.reflect.runtime.universe._
import scala.util.Random

object EPFDTester {

  val GLOBAL_STATE: String = "simulation.delivered"

  def apply[C <: ComponentDefinition: TypeTag](): collection.Iterable[ValidationGroup] = {
    val seed = 123;
    SimulationScenario.setSeed(seed);
    ValidationSimulation.runAndValidate(EPFDValidation(asJavaClass[C](typeOf[C]), 15, 3, 100L, 50, 200).scenario);
  }

  class EPFDInterceptor(init: Init[EPFDInterceptor]) extends ComponentDefinition {

    val epfd = requires[EventuallyPerfectFailureDetector];

    val self = init match {
      case Init(s: Address) => s
    };

    epfd uponEvent {
      case x: KompicsEvent => {
        //        println(s"Process $self triggers $x")
        val gv = cfg.getValue[GlobalView]("simulation.globalview");
        val globalState = gv.getValue(EPFDTester.GLOBAL_STATE, classOf[HashMap[Address, ListBuffer[KompicsEvent]]]);
        val myList = globalState.get(self).getOrElse(ListBuffer.empty[KompicsEvent])
        myList += (x);
        globalState.put(self, myList);
        gv.setValue(EPFDTester.GLOBAL_STATE, globalState);
      }
    }
  }

  class EPFDParent(init: Init[EPFDParent]) extends ComponentDefinition {

    val (addr, javaClass) = init match {
      case Init(s: Address, j: Class[_]) => (s, j.asInstanceOf[Class[ComponentDefinition]])
    };

    val network: PositivePort[Network] = requires[Network];
    val timer = requires[Timer];
    val pp2p = create(classOf[PerfectP2PLink], PerfectLinkInit(addr))
    val epfd = create(javaClass, Init[ComponentDefinition](addr));

    val interceptor = create(classOf[EPFDInterceptor], Init[EPFDInterceptor](addr));

    connect[Network](network -> pp2p);
    connect[PerfectLink](pp2p -> epfd);
    connect[Timer](timer -> epfd);
    connect[EventuallyPerfectFailureDetector](epfd -> interceptor);

  }

  /**
    * Eventually Perfect Failure Detector
    *
    * Properties Checked
    * ------------------
    *
    * EPFD1: Strong completeness: Eventually, every process that crashes is permanently suspected by every correct process.
    * EPFD2: Eventual strong accuracy: Eventually, no correct process is suspected by any correct process.
    *
    */
  case class EPFDValidation(compClass: Class[_ <: ComponentDefinition],
                            numNodes: Int,
                            discNodes: Int,
                            delay: Long,
                            delta: Long,
                            maxLinkDelay: Long) {

    import Distributions._

    assert(discNodes < numNodes)

    val topology = (1 to numNodes).map(AddressUtils.toAddress(_)).toList;
    val deathNote = getUniqueKillTargets(discNodes, numNodes);
    val correct = topology.filter(x => !deathNote.map(x => toAddress(x)).contains(x));
    //dynamic random for failed node selection
    val failed = deathNote.map(toAddress(_));

    def getUniqueKillTargets(size: Int, max: Int): Array[Integer] = {
      val extRand = new Random();
      assert(size < max);
      var myUniqueSet = Set.empty[Integer];
      while (myUniqueSet.size != size) {
        myUniqueSet.add(Integer.valueOf(extRand.nextInt(max) + 1));
      }
      myUniqueSet.toArray;
    }

    def toEntry(a: KompicsEvent): (Address, KompicsEvent) = {
      a match {
        case Suspect(nd) => (nd -> Suspect(nd));
        case Restore(nd) => (nd -> Restore(nd));
      }
    };

    def validateProperty(gv: GlobalView, suspected: Boolean): Boolean = {
      val globalState =
        gv.getValue(EPFDTester.GLOBAL_STATE, classOf[mutable.HashMap[Address, ListBuffer[KompicsEvent]]]);
      correct.forall(z => {
        val entry = globalState.get(z);
        val targetSet = if (suspected) failed.toList else correct;
        // if no failed process is never suspected that is bad, if a correct one is never restored or suspected that is ok...
        if (entry.isEmpty) {
          if (suspected) return false;
          else return true;
        }
        entry.get
          .foldLeft(HashMap.empty[Address, KompicsEvent])((b, a) => b += (toEntry(a)))
          .filter(next => targetSet.contains(next))
          .forall(proc => {
            if (entry.contains(proc._1)) {
              if (suspected) {
                proc._2.isInstanceOf[Suspect];
              } else {
                proc._2.isInstanceOf[Restore];
              }
            } else {
              if (suspected) false else true;
            }
          })
      });
    }

    val prop1: ValidationGroup = Property("Strong Completeness")
      .addValidator(
        SimValidator(
          gv => validateProperty(gv, true),
          100000L,
          "Every process that crashes is eventually suspected permanently by every correct process",
          "Not every process that crashes is eventually suspected permanently by every correct process"
        )
      )
      .build()

    val prop2: ValidationGroup = Property("Eventual Strong Accuracy")
      .addValidator(
        SimValidator(
          gv => validateProperty(gv, false),
          100000L,
          "Eventually, no correct process is suspected by any correct process",
          "There exist correct processes that were permanently suspected by correct processes"
        )
      )
      .build();

    //Simulation Operations ----
    val setupOp = Op(
      (_: Unit) =>
        Setup(
          (gv: GlobalView) =>
            gv.setValue(EPFDTester.GLOBAL_STATE, mutable.HashMap.empty[Address, ListBuffer[KompicsEvent]])
        ).build()
    );

    val setUniformDelay = () =>
      Op(
        (maxDelay: java.lang.Long) =>
          ChangeNetwork(NetworkModels.withUniformRandomDelay(0, maxDelay, SimulationScenario.getRandom))
      );

    val delayOp = Op(
      (_: Unit) =>
        ChangeNetwork(
          withDisconnectedModel(
            new SocketIdExtractor(),
            NetworkModels.withUniformRandomDelay(5, maxLinkDelay, SimulationScenario.getRandom),
            new java.util.HashSet[Identifier](
              topology.slice(0, discNodes).map(x => new SocketIdExtractor().extract(x)).toSet[Identifier].asJava
            )
          )
        )
    );

    /*
    val startEPFDs = Op((self: Integer) => StartNode(AddressUtils.toAddress(self),
      Init[EPFDParent](AddressUtils.toAddress(self), compClass),
      Map("epfd.simulation.topology" -> topology, "epfd.simulation.delay" -> delay, "epfd.simulation.delta" -> delta)));
     */

    val startEPFDs = Op { (self: Integer) =>
      val selfAddr = AddressUtils.toAddress(self);
      StartNode(selfAddr, Init[EPFDParent](AddressUtils.toAddress(self), compClass))
    }

    val killOP = Op { (self: Integer) =>
      KillNode(AddressUtils.toAddress(self))
    }

    val scenario =
      ValidationSimulation(prop1, prop2) inParallel (raise(1, setupOp).arrival(Distributions.constant(0)), raise(
        1,
        setUniformDelay(),
        constant(maxLinkDelay)
      ).arrival(constant(0)),
      raise(numNodes, startEPFDs, 1.toN).arrival(constant(0))) andThen 10000.millis afterTermination
        raise(1, delayOp).arrival(constant(0)) andThen 500.millis afterTermination
        //      raise(1, setUniformDelay(), constant(0)).arrival(constant(0)) andThen 10000.millis afterTermination
        raise(discNodes, killOP, new GenIntSequentialDistribution(deathNote))
          .arrival(constant(0)) andThen 100000L.millis afterTermination Terminate;
  }

}
