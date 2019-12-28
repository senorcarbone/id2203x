package se.kth.edx.id2203.validation.testers

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.ExercisePrimitives.FifoP2PLink.FifoLinkInit
import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink._
import se.kth.edx.id2203.core.ExercisePrimitives.{AddressUtils, FifoP2PLink, PerfectP2PLink}
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.core.SimulationPrimitives.{ProxyInterceptor, ProxyInterceptorInit, ScheduleTriggers}
import se.kth.edx.id2203.validation.ValidationUtils.{Property, SimValidator, ValidationGroup, ValidationSimulation}
import se.kth.edx.id2203.validation.testers.TesterPrimitives.TesterPrimitives._
import se.sics.kompics.network.{Address, Network}
import se.sics.kompics.simulator.SimulationScenario
import se.sics.kompics.simulator.network.identifier.Identifier
import se.sics.kompics.simulator.network.identifier.impl.SocketIdExtractor
import se.sics.kompics.simulator.network.impl.NetworkModels
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.sl.simulator._
import se.sics.kompics.sl.{ComponentDefinition, Init, _}
import se.sics.kompics.timer.{ScheduleTimeout, Timeout, Timer}
import se.sics.kompics.{KompicsEvent, Start}

import scala.jdk.CollectionConverters._
import scala.collection.immutable.Set
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, _}
import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.Random

object SequenceConsensusTester {

  case class RSM_SimCmd(id: Long) extends RSM_Command;

  val LOG_GLOBAL: String = "simulation.globalview"
  val LOG_DECIDED: String = "simulation.decided"
  val LOG_PROPOSED: String = "simulation.proposed"

  def apply[CTYPE <: ComponentDefinition: TypeTag](): collection.Iterable[ValidationGroup] = {
    SimulationScenario.setSeed(1334)
    ValidationSimulation.runAndValidate(
      SeqConsensusValidation[CTYPE](15, 6, 2, 200L, 400L, 5000L, 1, 10L, 300L, 100).scenario
    );
  }

  class InterceptorPayload(id: Int) extends KompicsEvent with Serializable;

  // - Program interfaces for Simulation
  case class ProposalTimeout(timeout: ScheduleTimeout) extends Timeout(timeout)

  case class ProposedCommand(cmd: RSM_SimCmd) extends KompicsEvent;

  class SeqConsensusInterceptor(init: Init[SeqConsensusInterceptor]) extends ProxyInterceptor[SequenceConsensus] {

    val seqconsensus = requires[SequenceConsensus];
    val leadelection = requires[BallotLeaderElection];
    val pp2p = requires[PerfectLink];
    var isLeader = false;
    val r = Random;
    val propRate = cfg.getValue[Long]("sc.simulation.rate");
    val maxProposals = cfg.getValue[Int]("sc.simulation.maxProposals");
    var numProposals = 0;
    var currentCmd: Option[RSM_SimCmd] = None;

    def scheduleProposal(): Unit = {
      val timeout = new ScheduleTimeout(propRate);
      timeout.setTimeoutEvent(ProposalTimeout(timeout));
      trigger(timeout -> timer);
    }

    val (self, pInit, topology, correct) = init match {
      case Init(s: Address,
                proxyInit: ProxyInterceptorInit,
                topol: Set[Address] @unchecked,
                corr: Set[Address] @unchecked) =>
        (s, proxyInit, topol, corr)
    }

    ctrl uponEvent {
      case _: Start => {
        initialize(pInit);
        if (correct.contains(self)) scheduleProposal();
      }
    }

    pp2p uponEvent {
      case PL_Deliver(_, ProposedCommand(nextCommand)) => {
        trigger(SC_Propose(nextCommand) -> seqconsensus);
      }
    }

    timer uponEvent {
      case ProposalTimeout(_) => {
        if (numProposals <= maxProposals) {
          if (currentCmd == None) {
            currentCmd = Some(RSM_SimCmd(r.nextLong()));
            val gv = cfg.getValue[GlobalView](LOG_GLOBAL);
            val proposed = gv.getValue(LOG_PROPOSED, classOf[scala.collection.mutable.Set[RSM_SimCmd]]);
            proposed += (currentCmd.get);
            numProposals = numProposals + 1;
          }
          topology.foreach(p => trigger(PL_Send(p, ProposedCommand(currentCmd.get)) -> pp2p));
          scheduleProposal();
        }
      }
    }

    var curLeader = AddressUtils.toRank(self);

    leadelection uponEvent {
      case BLE_Leader(newLeader, blt) => {
        curLeader = AddressUtils.toRank(newLeader);
      }
    }

    seqconsensus uponEvent {
      case x @ SC_Decide(y @ RSM_SimCmd(_)) => {
        val gv = cfg.getValue[GlobalView](LOG_GLOBAL);
        val scLog = gv.getValue(LOG_DECIDED, classOf[HashMap[Address, ListBuffer[RSM_SimCmd]]]);
        val appendLog = scLog.get(self).getOrElse(ListBuffer.empty[RSM_SimCmd]);
        appendLog += (y);
        scLog.put(self, appendLog);
        gv.setValue(LOG_DECIDED, scLog);

        if (currentCmd.isDefined && currentCmd.get == y) {
          currentCmd = None;
        }
//        println(s" ${System.currentTimeMillis()} :: ${AddressUtils.toRank(self)} decided: ${appendLog.size}: $y- lead: ${curLeader}");
      }
    }

    override def getInterceptedPort(): PositivePort[SequenceConsensus] = seqconsensus;

    override def getSelfAddress(): Address = self
  }

  class SeqConsensusParent(init: Init[SeqConsensusParent]) extends ComponentDefinition {

    val (selfAddr, topology, correct, paxosClass, proxyInit) = init match {
      case Init(s: Address,
                t: Set[Address] @unchecked,
                c: Set[Address] @unchecked,
                pClass: Class[ComponentDefinition] @unchecked,
                m: ProxyInterceptorInit) =>
        (s, t, c, pClass, m)
    };

    val timer = requires[Timer];
    val network = requires[Network];

    val pLink = create(classOf[PerfectP2PLink], PerfectLinkInit(selfAddr));
    val fifoLink = create(classOf[FifoP2PLink], FifoLinkInit(selfAddr, topology));

    val leaderElection = create(classOf[ProvidedLeaderElection], Init[ProvidedLeaderElection](selfAddr))
    val seqConsensus = create(paxosClass, Init[ComponentDefinition](selfAddr, topology));
    val interceptor =
      create(classOf[SeqConsensusInterceptor], Init[SeqConsensusInterceptor](selfAddr, proxyInit, topology, correct));

    connect[Network](network -> pLink);
    connect[Timer](timer -> interceptor);
    connect[Timer](timer -> leaderElection)

    connect[PerfectLink](pLink -> interceptor)
    connect[PerfectLink](pLink -> fifoLink);
    connect[PerfectLink](pLink -> leaderElection);
    connect[FIFOPerfectLink](fifoLink -> seqConsensus);

    connect[BallotLeaderElection](leaderElection -> interceptor);
    connect[BallotLeaderElection](leaderElection -> seqConsensus);
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
  case class SeqConsensusValidation[CTYPE <: ComponentDefinition: TypeTag](numNodes: Int,
                                                                           numFailed: Int,
                                                                           numDisconnected: Int,
                                                                           latencyMin: Long,
                                                                           latencyMax: Long,
                                                                           splitNetworkDelay: Long,
                                                                           ble_Delay: Long,
                                                                           ble_Delta: Long,
                                                                           sc_Rate: Long,
                                                                           sc_maxProp: Int) {

    import Distributions._

    assert(numNodes > numFailed);

    //this is a true random (not using the simulator seed) to ensure uniqueness across validations
    val instantRandom = new Random();

    val topology = (1 to numNodes).map(toAddress(_)).toList;
    val sorted = topology.map(x => (x, x.hashCode())).sortBy(_._2).map(_._1);
    val failed = sorted.takeRight(numFailed)
    val correct = sorted.take(numNodes - numFailed)

    def setupEvents(gv: GlobalView): Unit = {
      gv.setValue(LOG_DECIDED, HashMap.empty[Address, ListBuffer[SC_Decide]]);
      gv.setValue(LOG_PROPOSED, scala.collection.mutable.Set.empty[RSM_SimCmd]);
    }

    //Simulation Operations ----
    val setupOp = Op((_: Unit) => Setup(setupEvents _).build());

    val setConstantLatencyNetwork = () =>
      Op((maxDelay: java.lang.Long) => ChangeNetwork(NetworkModels.withConstantDelay(maxDelay)));
    val setUniformLatencyNetwork = () =>
      Op(
        (minDelay: java.lang.Long, maxDelay: java.lang.Long) =>
          ChangeNetwork(NetworkModels.withUniformRandomDelay(minDelay, maxDelay))
      );

    val setUniformDelay = () =>
      Op(
        (minDelay: java.lang.Long, maxDelay: java.lang.Long) =>
          ChangeNetwork(NetworkModels.withUniformRandomDelay(minDelay, maxDelay, SimulationScenario.getRandom))
      );

    val splitNetwork = () =>
      Op(
        (_: Unit) =>
          ChangeNetwork(
            NetworkModels.withBinaryModel(
              new SocketIdExtractor(),
              NetworkModels.withUniformRandomDelay(latencyMin, latencyMax, SimulationScenario.getRandom),
              NetworkModels.withConstantDelay(splitNetworkDelay),
              new java.util.HashSet[Identifier](
                topology
                  .slice(topology.size - failed.size - numDisconnected, topology.size - failed.size + 1)
                  .map(x => new SocketIdExtractor().extract(x))
                  .toSet[Identifier]
                  .asJava
              )
            )
          )
      );

    val killOP = Op { (self: Integer) =>
      KillNode(sorted(self.toInt))
    }

    // Old code that was not compiling.
    // TODO: Verify that the Map is not needed any longer...
    /*
    val startPaxos = Op((self: Integer) => {
      StartNode(toAddress(self), Init[SeqConsensusParent](toAddress(self),
        topology.toSet, correct.toSet,
        asJavaClass[CTYPE](typeOf[CTYPE]), ScheduleTriggers(toAddress(self))()),
        Map("ble.simulation.topology" -> topology.toSet,
          "ble.simulation.delay" -> ble_Delay,
          "ble.simulation.delta" -> ble_Delta,
          "sc.simulation.rate" -> sc_Rate,
          "sc.simulation.maxProposals" -> sc_maxProp))
    });
     */

    val startPaxos = Op { (self: Integer) =>
      val selfAddr = AddressUtils.toAddress(self);
      StartNode(selfAddr,
                Init[SeqConsensusParent](selfAddr,
                                         topology.toSet,
                                         correct.toSet,
                                         asJavaClass[CTYPE](typeOf[CTYPE]),
                                         ScheduleTriggers(selfAddr)()))
    }

    def checkValidity(view: GlobalView): Boolean = {
      val proposed = view.getValue(LOG_PROPOSED, classOf[scala.collection.mutable.Set[RSM_SimCmd]]);
      view
        .getValue(LOG_DECIDED, classOf[HashMap[Address, ListBuffer[RSM_SimCmd]]])
        .forall(_._2.forall(x => proposed.contains(x)));
    }

    def checkAggreement(view: GlobalView): Boolean = {
      val decided = view.getValue(LOG_DECIDED, classOf[mutable.HashMap[Address, ListBuffer[RSM_SimCmd]]])
      if (!decided.isEmpty) {
        val maxTpl = decided.maxBy(_._2.size);
        return decided.forall { x =>
          x._2.toList == maxTpl._2.slice(0, x._2.size).toList
        };
        //return decided.forall { x => x._2.toList == maxTpl._2.subList(0, x._2.size).toList };
      }
      return true;
    }

    def checkCompleteness(view: GlobalView): Boolean = {
      val proposed = view.getValue(LOG_PROPOSED, classOf[scala.collection.mutable.Set[RSM_SimCmd]]);
      val decided = view.getValue(LOG_DECIDED, classOf[mutable.HashMap[Address, ListBuffer[RSM_SimCmd]]])

      return correct.forall(p => decided.get(p).isDefined) &&
        correct.forall(p => proposed.subsetOf(decided.get(p).get.toSet))
    }

    val prop1 =
      Property("Validity")
        .addValidator(
          SimValidator(
            checkValidity,
            500000,
            "Every value decided in a sequence has been previously proposed",
            "There exist values decided in a sequence that were not previously proposed"
          )
        )
        .build();

    val prop2 =
      Property("Uniform Agreement")
        .addValidator(
          SimValidator(
            checkAggreement,
            500000,
            "For every two sequences decided across different processes, one is a prefix of the other",
            "There exist at least one decided sequence which is not a prefix of another"
          )
        )
        .build();

    //    val prop3 =
    //      Property("Integrity")
    //        .addValidator(SimValidator( checkIntegrity, 500000,
    //          "Every decided sequence is a strict prefix of next decided sequence by a process",
    //          "Not every decided sequence is a strict prefix of the next decided sequence by a process")).build();

    val prop4 =
      Property("Completeness-Termination")
        .addValidator(
          SimValidator(
            checkCompleteness,
            500000,
            "If a command is proposed an infinite number of times by a correct process then it is eventually included at least once in a decided sequence by every correct process",
            "There exist commands proposed an infinite number of times by a correct process which never get included in a process' decided sequence"
          )
        )
        .build();

    val scenario =
      ValidationSimulation(prop1, prop2, prop4) inParallel (raise(1, setupOp).arrival(Distributions.constant(0)), raise(
        1,
        setUniformDelay(),
        constant(latencyMin),
        constant(latencyMax)
      ).arrival(constant(0)),
      raise(numNodes, startPaxos, 1.toN).arrival(constant(0))) andThen 4000.millis afterTermination
        raise(numFailed, killOP, Distributions.intSeq(((numNodes - numFailed) until numNodes)))
          .arrival(constant(0)) andThen 5000.millis afterTermination
        raise(1, splitNetwork()).arrival(constant(0)) andThen 4000.millis afterTermination
        raise(1, setUniformDelay(), constant(latencyMin), constant(latencyMax))
          .arrival(constant(0)) andThen 10000000L.millis afterTermination Terminate;
  }

}
