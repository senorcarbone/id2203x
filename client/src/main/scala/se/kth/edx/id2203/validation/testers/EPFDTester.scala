package se.kth.edx.id2203.validation.testers

import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink.PerfectLinkInit
import se.kth.edx.id2203.core.ExercisePrimitives.{AddressUtils, PerfectP2PLink, PerfectP2PLink$}
import se.kth.edx.id2203.core.Ports.{EventuallyPerfectFailureDetector, PerfectLink, Restore, Suspect}
import se.kth.edx.id2203.validation.ValidationUtils.{Property, SimValidator, ValidationGroup, ValidationSimulation}
import se.sics.kompics.network.{Address, Network}
import se.sics.kompics.simulator.network.identifier.Identifier
import se.sics.kompics.simulator.network.identifier.impl.SocketIdExtractor
import se.sics.kompics.simulator.network.impl.NetworkModels
import se.sics.kompics.simulator.network.impl.NetworkModels._
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.simulator.{SimulationScenario => JSimulationScenario}
import se.sics.kompics.simulator.SimulationScenario
import se.sics.kompics.sl._
import se.sics.kompics.sl.Init
import se.sics.kompics.sl.ComponentDefinition
import se.sics.kompics.sl.simulator._
import se.sics.kompics.timer.Timer

import scala.collection.JavaConversions._
import scala.collection.mutable._
import scala.concurrent.duration._
import scala.reflect.runtime.universe._

object EPFDTester {

  def apply[C <: ComponentDefinition : TypeTag]() : collection.Iterable[ValidationGroup] = {
    val seed = 123;
    JSimulationScenario.setSeed(seed);
    ValidationSimulation.runAndValidate(EPFDValidation[C](3, 2, 10, 5, 20).scenario);
  }

  class EPFDInterceptor(init: Init[EPFDInterceptor]) extends ComponentDefinition {

    val epfd = requires[EventuallyPerfectFailureDetector];

    val self = init match {
      case Init(s: Address) => s
    };

    epfd uponEvent {
      case Restore(addr) => handle {
        //FIXME
        println(s"Restore:$self -> $addr :: ${System.currentTimeMillis()}");
        //TODO update global state here
      }
      case Suspect(addr) => handle {
        //FIXME
        println(s"Suspect:$self -> $addr :: ${System.currentTimeMillis()}");
      }
    }

    def logSuspect(): Unit = {
      val gv = cfg.getValue[GlobalView]("simulation.globalview");
      gv.setValue("simulation.suspects", gv.getValue("simulation.suspects", classOf[Integer]) + 1);
    }

  }

  class EPFDParent(init: Init[EPFDParent]) extends ComponentDefinition {
    
    val (addr, javaClass) = init match { case Init(s: Address, j: Class[_]) => (s,j.asInstanceOf[Class[ComponentDefinition]])};
    
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

  case class EPFDValidation[C <: ComponentDefinition : TypeTag](numNodes: Int, discNodes: Int, delay: Long, delta: Long, maxLinkDelay: Long) {

    import Distributions._

    val topology = (1 to numNodes).map(AddressUtils.toAddress(_)).toList;

    //FIXME
    val prop: ValidationGroup = Property("Eventual Correctness")
      .addValidator(SimValidator((_.getDeadNodes.size() == 2), 15000, "2 nodes are killed", "no correct nodes are killed")).build();


    //Simulation Operations ----
    val setupOp = Op((_: Unit) => Setup((gv: GlobalView) => gv.setValue("simulation.suspects", 0)).build());

    val setNormalLossyNetwork = () => Op((maxDelay: java.lang.Long) =>
      ChangeNetwork(NetworkModels.withUniformRandomDelay(0, maxDelay, SimulationScenario.getRandom)));

    val delayOp = Op((_: Unit) => ChangeNetwork(withDisconnectedModel(
      new SocketIdExtractor(), NetworkModels.withUniformRandomDelay(0, maxLinkDelay, SimulationScenario.getRandom),
      new java.util.HashSet[Identifier](topology.slice(0, discNodes).map(x => new SocketIdExtractor().extract(x)).toSet[Identifier]))));

    val startEPFDs = Op((self: Integer) => StartNode(AddressUtils.toAddress(self), Init[EPFDParent](AddressUtils.toAddress(self), asJavaClass[C](typeOf[C])),
      Map("epfd.simulation.topology" -> topology, "epfd.simulation.delay" -> delay, "epfd.simulation.delta" -> delta)));

    val killOP = Op { (self: Integer) => KillNode(AddressUtils.toAddress(self)) }

    val scenario =
      ValidationSimulation(prop) inParallel(
        raise(1, setupOp).arrival(Distributions.constant(0)), raise(1, setNormalLossyNetwork(), constant(0l)).arrival(constant(0)),
        raise(numNodes, startEPFDs, 1.toN).arrival(constant(0))) andThen 10000.millis afterTermination
        raise(1, delayOp).arrival(constant(0)) andThen 500.millis afterTermination
        raise(1, setNormalLossyNetwork(), constant(0)).arrival(constant(0)) andThen 10000.millis afterTermination
        raise(numNodes - discNodes, killOP, discNodes.toN).arrival(constant(0)) andThen 100000.millis afterTermination Terminate;
  }

}

