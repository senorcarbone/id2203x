package se.kth.edx.id2203.validation.testers


import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.ExercisePrimitives.FifoP2PLink.FifoLinkInit
import se.kth.edx.id2203.core.ExercisePrimitives.{FifoP2PLink, PerfectP2PLink}
import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink._
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.core.SimulationPrimitives.{ProxyInterceptor, ProxyInterceptorInit, ScheduleTriggers}
import se.kth.edx.id2203.validation.ValidationUtils.{Property, SimValidator, ValidationGroup, ValidationSimulation}
import se.kth.edx.id2203.validation.testers.TesterPrimitives.TesterPrimitives._
import se.sics.kompics.network.{Address, Network}
import se.sics.kompics.simulator.SimulationScenario
import se.sics.kompics.simulator.network.impl.NetworkModels
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.sl.simulator._
import se.sics.kompics.sl.{ComponentDefinition, Init, _}
import se.sics.kompics.timer.Timer
import se.sics.kompics.{KompicsEvent, Start}

import scala.collection.JavaConversions._
import scala.collection.immutable.Set
import scala.collection.mutable.{ListBuffer, _}
import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.Random

object SequenceConsensusTester {

  val LOG_STATE: String = "simulation.delivered"

  def apply[CTYPE <: ComponentDefinition: TypeTag](): collection.Iterable[ValidationGroup] = {
    SimulationScenario.setSeed(94305)
    ValidationSimulation.runAndValidate(SeqConsensusValidation[CTYPE](5, 3, 20l, 25l, 1,  100l, 50).scenario);
  }

  class InterceptorPayload(id: Int) extends KompicsEvent with Serializable;

  // - Program interfaces for Simulation

  case class BroadcastedMessage(src: Address, seq: Int) extends InterceptorPayload(seq);

  class SeqConsensusInterceptor(init: Init[SeqConsensusInterceptor]) extends ProxyInterceptor[SequenceConsensus] {

    val (self, pInit) = init match {
      case Init(s: Address, proxyInit: ProxyInterceptorInit) => (s, proxyInit)
    }

    val seqconsensus = requires[SequenceConsensus];

    ctrl uponEvent {
      case _: Start => handle {
        initialize(pInit);
      }
    }

    seqconsensus uponEvent {
      case x @ SC_Decide (y) => handle{
//        val gv = cfg.getValue[GlobalView]("simulation.globalview");
//        val stateLogs = gv.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[C_Decide]]]);
//        val myList = stateLogs.get(self).getOrElse(ListBuffer.empty[C_Decide]);
//        myList.add(C_Decide(ValidationUtils.unfoldValue(y)));
//        stateLogs.put(self, myList);
//        gv.setValue(LOG_STATE, stateLogs);
      }
    }

    override def getInterceptedPort(): PositivePort[SequenceConsensus] = seqconsensus;

    override def getSelfAddress(): Address = self
  }

  class SeqConsensusParent(init: Init[SeqConsensusParent]) extends ComponentDefinition {

    val (selfAddr, topology, paxosClass, proxyInit) = init match {
      case Init(s: Address, t: Set[Address] @unchecked, pClass: Class[ComponentDefinition] @unchecked,
        m: ProxyInterceptorInit) => (s, t, pClass, m)
    };

    val timer = requires[Timer];
    val network = requires[Network];

    val pLink = create(classOf[PerfectP2PLink], PerfectLinkInit(selfAddr));
    val fifoLink = create(classOf[FifoP2PLink], FifoLinkInit(selfAddr, topology));
    
    val leaderElection = create(classOf[ProvidedLeaderElection], Init[ProvidedLeaderElection](selfAddr))
    val seqConsensus = create(paxosClass, Init[ComponentDefinition](selfAddr, topology));
    val interceptor = create(classOf[SeqConsensusInterceptor], Init[SeqConsensusInterceptor](selfAddr, proxyInit));

    connect[Network](network -> pLink);
    connect[Timer](timer -> interceptor);
    connect[Timer](timer -> leaderElection)
    
    connect(PerfectLink)(pLink -> leaderElection);
    connect[FIFOPerfectLink](fifoLink -> seqConsensus);

    connect(BallotLeaderElection)(leaderElection -> seqConsensus);
    connect[SequenceConsensus](seqConsensus -> interceptor);
  }

  /**
   * Sequence Consensus
   *
   * Properties Checked
   * ------------------
   *
   *
   */

  case class SeqConsensusValidation[CTYPE <: ComponentDefinition: TypeTag](numNodes: Int, numProposers: Int, latencyMin: Long, latencyMax: Long, skew: Int, ble_Delay: Long, ble_Delta: Long) {

    import Distributions._

    assert(numNodes > numProposers);

    //this is a true random (not using the simulator seed) to ensure uniqueness across validations
    val instantRandom = new Random();
    val msgs = Seq.fill(numProposers)(C_Propose(instantRandom.nextInt)).toArray[C_Propose]

    val topology = (1 to numNodes).map(toAddress(_)).toSet;

    //Simulation Operations ----
    val setupOp = Op((_: Unit) => Setup((gv: GlobalView) => gv.setValue(LOG_STATE, HashMap.empty[Address, ListBuffer[C_Decide]])).build());

    val setConstantLatencyNetwork = () => Op((maxDelay: java.lang.Long) => ChangeNetwork(NetworkModels.withConstantDelay(maxDelay)));
    val setUniformLatencyNetwork = () => Op((minDelay: java.lang.Long, maxDelay: java.lang.Long) => ChangeNetwork(NetworkModels.withUniformRandomDelay(minDelay, maxDelay)));

    val triggers = (1 to numProposers).map(x => (toAddress(x), Int.box(x * skew).toLong, msgs(x - 1))).toArray
    val startPaxos = Op((self: Integer) => {
      StartNode(toAddress(self), Init[SeqConsensusParent](toAddress(self), topology,
        asJavaClass[CTYPE](typeOf[CTYPE]), ScheduleTriggers(toAddress(self))(triggers: _*)),
        Map("ble.simulation.topology" -> topology, 
          "ble.simulation.delay" -> ble_Delay, 
          "ble.simulation.delta" -> ble_Delta))
    });

    val prop1 =
      Property("Validity")
        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[C_Decide]]])
          .forall(_._2.forall(x => msgs.map(_.value).contains(x.value))), 109999,
          "Every value decided has been previously proposed",
          "There exist values decided that were not previously proposed")).build();

//    val prop2 =
    //      Property("Uniform Agreement")
    //        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[C_Decide]]])
    //          .groupBy(_._2).size == 1, 109999,
    //          "There were no two nodes deciding different values",
    //          "There were different decided values among different nodes")).build();

    val prop3 =
      Property("Integrity")
        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[C_Decide]]])
          .forall(_._2.size < 2), 109999,
          "Each node decided a value at most once",
          "There were nodes that decided more than once")).build();

    val prop4 =
      Property("Termination")
        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[C_Decide]]])
          .forall(_._2.size != 0), 109999,
          "Each node eventually decides",
          "The decision process might not terminate")).build();

    val scenario =
      ValidationSimulation(prop1, prop3, prop4) inParallel (
        raise(1, setupOp).arrival(Distributions.constant(0l)), raise(1, setUniformLatencyNetwork(),
          constant(latencyMin), constant(latencyMax)).arrival(constant(0)),
          raise(numNodes, startPaxos, 1.toN).arrival(constant(0l))) andThen 1000000.millis afterTermination Terminate;
  }

}

//"ble.simulation.topology" -> topology, "ble.simulation.delay" -> delay, "ble.simulation.delta" -> delta