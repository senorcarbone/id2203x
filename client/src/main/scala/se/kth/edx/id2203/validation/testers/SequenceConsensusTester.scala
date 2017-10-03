package se.kth.edx.id2203.validation.testers


import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.ExercisePrimitives.FifoP2PLink.FifoLinkInit
import se.kth.edx.id2203.core.ExercisePrimitives.{AddressUtils, FifoP2PLink, PerfectP2PLink}
import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink._
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.core.SimulationPrimitives.{ProxyInterceptor, ProxyInterceptorInit, ScheduleTriggers}
import se.kth.edx.id2203.validation.ValidationUtils.{Property, SimValidator, ValidationGroup, ValidationSimulation}
import se.kth.edx.id2203.validation.testers.TesterPrimitives.TesterPrimitives._
import se.sics.kompics.network.{Address, Network}
import se.sics.kompics.simulator.SimulationScenario
import se.sics.kompics.simulator.network.identifier.Identifier
import se.sics.kompics.simulator.network.identifier.impl.SocketIdExtractor
import se.sics.kompics.simulator.network.impl.NetworkModels
import se.sics.kompics.simulator.network.impl.NetworkModels.withDisconnectedModel
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.sl.simulator._
import se.sics.kompics.sl.{ComponentDefinition, Init, _}
import se.sics.kompics.timer.{ScheduleTimeout, Timeout, Timer}
import se.sics.kompics.{KompicsEvent, Start}

import scala.collection.JavaConversions._
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
  val LOG_PROPOSED : String = "simulation.proposed"

  def apply[CTYPE <: ComponentDefinition: TypeTag](): collection.Iterable[ValidationGroup] = {
    SimulationScenario.setSeed(943505)
    ValidationSimulation.runAndValidate(SeqConsensusValidation[CTYPE](15, 5, 2, 100l, 300l, 5000l, 1,  10l, 300l, 100).scenario);
  }

  class InterceptorPayload(id: Int) extends KompicsEvent with Serializable;

  // - Program interfaces for Simulation

  case class BroadcastedMessage(src: Address, seq: Int) extends InterceptorPayload(seq);

  case class ProposalTimeout(timeout: ScheduleTimeout) extends Timeout(timeout)
  
  class SeqConsensusInterceptor(init: Init[SeqConsensusInterceptor]) extends ProxyInterceptor[SequenceConsensus] {

    val seqconsensus = requires[SequenceConsensus];
    val leadelection = requires(BallotLeaderElection);
    var isLeader = false;
    val r = Random;
    val propRate =  cfg.getValue[Long]("sc.simulation.rate");
    val maxProposals = cfg.getValue[Int]("sc.simulation.maxProposals"); 
    var numProposals = 0;

    def storeNext(nextCmd: RSM_SimCmd) {
      val gv = cfg.getValue[GlobalView](LOG_GLOBAL);
      val proposed = gv.getValue(LOG_PROPOSED, classOf[scala.collection.mutable.Set[RSM_SimCmd]]);
      proposed.add(nextCmd);
    }
    
    def scheduleProposal(): Unit = {
      val timeout = new ScheduleTimeout(propRate);
      timeout.setTimeoutEvent(ProposalTimeout(timeout));
      trigger(timeout -> timer);
    }
    
    val (self, pInit) = init match {
      case Init(s: Address, proxyInit: ProxyInterceptorInit) => (s, proxyInit)
    }

    ctrl uponEvent {
      case _: Start => handle {
        initialize(pInit);
      }
    }

    timer uponEvent {
      case ProposalTimeout(_) => handle {
        if(isLeader && numProposals <= maxProposals){
          val nextCmd = RSM_SimCmd(r.nextLong());
          storeNext(nextCmd);
          println(s" ${System.currentTimeMillis()} :: ${AddressUtils.toRank(self)} proposing ${nextCmd.id.toByte}. This is its $numProposals proposal as a leader");
          trigger(SC_Propose(nextCmd) -> seqconsensus);
          numProposals = numProposals + 1;
          scheduleProposal();
        }
        else
          {
           numProposals = 0; 
          }
      }
    }

    var curLeader = AddressUtils.toRank(self);

    leadelection uponEvent {
      case BLE_Leader(newLeader, blt) => handle{
        curLeader = AddressUtils.toRank(newLeader);
        println(s" ${System.currentTimeMillis()} :: ${AddressUtils.toRank(self)} new leader:  $curLeader with ballot $blt");
        
        if(newLeader == self && !isLeader){
          scheduleProposal();
          numProposals = numProposals + 1;
        }
        
        isLeader = (newLeader == self); 
      }
    }

    seqconsensus uponEvent {
      case x @ SC_Decide (y @ RSM_SimCmd(_)) => handle{
        val gv = cfg.getValue[GlobalView](LOG_GLOBAL);
        val scLog = gv.getValue(LOG_DECIDED, classOf[HashMap[Address, ListBuffer[RSM_SimCmd]]]);
        val appendLog = scLog.get(self).getOrElse(ListBuffer.empty[RSM_SimCmd]);
        appendLog.add(y);
        scLog.put(self, appendLog);
        //DEBUG
        println(s" ${System.currentTimeMillis()} :: ${AddressUtils.toRank(self)} decided ${y.id.toByte}  [length : ${appendLog.size}, leader : ${curLeader}]");
        //DEBUG
        gv.setValue(LOG_DECIDED, scLog);
      }
    }

    override def getInterceptedPort(): PositivePort[SequenceConsensus] = seqconsensus;

    override def getSelfAddress(): Address = self
  }

  class SeqConsensusParent(init: Init[SeqConsensusParent]) extends ComponentDefinition {

    import Distributions._
    
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
    connect(PerfectLink)(pLink -> fifoLink);
    connect[FIFOPerfectLink](fifoLink -> seqConsensus);

    connect(BallotLeaderElection)(leaderElection -> interceptor);
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

  case class SeqConsensusValidation[CTYPE <: ComponentDefinition: TypeTag](numNodes: Int, numFailed: Int, numDisconnected : Int, latencyMin: Long, latencyMax: Long, splitNetworkDelay : Long, ble_Delay: Long, ble_Delta: Long, sc_Rate : Long, sc_maxProp: Int) {

    import Distributions._

    assert(numNodes > numFailed);

    //this is a true random (not using the simulator seed) to ensure uniqueness across validations
    val instantRandom = new Random();
    
    val topology = (1 to numNodes).map(toAddress(_)).toList;
    val sorted = topology.map(x => (x, x.hashCode())).sortBy(_._2).map(_._1);
    val failed = sorted.takeRight(numFailed)
    val correct = sorted.take(numNodes-numFailed)
    

    def setupEvents(gv: GlobalView) : Unit =
    {
      gv.setValue(LOG_DECIDED, HashMap.empty[Address, ListBuffer[SC_Decide]]);
      gv.setValue(LOG_PROPOSED, scala.collection.mutable.Set.empty[RSM_SimCmd]);
    }
    
    //Simulation Operations ----
    val setupOp = Op((_: Unit) => Setup(setupEvents _).build());

    val setConstantLatencyNetwork = () => Op((maxDelay: java.lang.Long) => ChangeNetwork(NetworkModels.withConstantDelay(maxDelay)));
    val setUniformLatencyNetwork = () => Op((minDelay: java.lang.Long, maxDelay: java.lang.Long) => ChangeNetwork(NetworkModels.withUniformRandomDelay(minDelay, maxDelay)));

    val setUniformDelay = () => Op((minDelay: java.lang.Long, maxDelay: java.lang.Long) =>
      ChangeNetwork(NetworkModels.withUniformRandomDelay(minDelay, maxDelay, SimulationScenario.getRandom)));

    
    val splitNetwork = () => Op((_: Unit) => ChangeNetwork(
      NetworkModels.withBinaryModel(new SocketIdExtractor(),
        NetworkModels.withUniformRandomDelay(latencyMin, latencyMax, SimulationScenario.getRandom),
        NetworkModels.withConstantDelay(splitNetworkDelay), 
        new java.util.HashSet[Identifier](topology.slice(topology.size-failed.size-numDisconnected, topology.size-failed.size+1).map(x => new SocketIdExtractor().extract(x)).toSet[Identifier]))));

    val killOP = Op { (self: Integer) => KillNode(sorted(self.toInt)) }

    val startPaxos = Op((self: Integer) => {
      StartNode(toAddress(self), Init[SeqConsensusParent](toAddress(self), topology.toSet,
        asJavaClass[CTYPE](typeOf[CTYPE]), ScheduleTriggers(toAddress(self))()),
        Map("ble.simulation.topology" -> topology.toSet, 
          "ble.simulation.delay" -> ble_Delay, 
          "ble.simulation.delta" -> ble_Delta,
          "sc.simulation.rate" -> sc_Rate,
          "sc.simulation.maxProposals" -> sc_maxProp))
    });
    
    def checkValidity(view: GlobalView) : Boolean ={
      val proposed = view.getValue(LOG_PROPOSED, classOf[scala.collection.mutable.Set[RSM_SimCmd]]);
      view.getValue(LOG_DECIDED, classOf[HashMap[Address, ListBuffer[RSM_SimCmd]]])
        .forall(_._2.forall(x => proposed.contains(x)));
    }
    
    def checkAggreement(view: GlobalView) : Boolean ={
      val decided = view.getValue(LOG_DECIDED, classOf[mutable.HashMap[Address, ListBuffer[RSM_SimCmd]]])
      val maxTpl = decided.maxBy(_._2.size);
      decided.forall { x => x._2.toList == maxTpl._2.subList(0, x._2.size).toList};
    }
    
    val prop1 =
      Property("Validity")
        .addValidator(SimValidator(checkValidity, 500000,
          "Every value decided has been previously proposed",
          "There exist values decided that were not previously proposed")).build();

    val prop2 =
      Property("Uniform Agreement")
        .addValidator(SimValidator( checkAggreement, 500000,
          "There were no two processes deciding different sequences",
          "Decided value sequences do not match across different processes")).build();
    
    val prop3 =
      Property("Termination")
        .addValidator(SimValidator(_.getValue(LOG_DECIDED, classOf[HashMap[Address, ListBuffer[C_Decide]]])
          .forall(_._2.size != 0), 500000,
          "Each node eventually decides on a sequence",
          "The decision process might not terminate")).build();
    

    val scenario =
      ValidationSimulation(prop1, prop2, prop3) inParallel(
        raise(1, setupOp).arrival(Distributions.constant(0)), raise(1, setUniformDelay(), constant(latencyMin), constant(latencyMax)).arrival(constant(0)),
        raise(numNodes, startPaxos, 1.toN).arrival(constant(0))) andThen 4000.millis afterTermination
        raise(numFailed, killOP, Distributions.intSeq(((numNodes-numFailed) until numNodes))).arrival(constant(0)) andThen 5000.millis afterTermination
        raise(1, splitNetwork()).arrival(constant(0)) andThen 4000.millis afterTermination 
        raise(1, setUniformDelay(), constant(latencyMin), constant(latencyMax)).arrival(constant(0))  andThen 10000000l.millis afterTermination Terminate;
  }

}
