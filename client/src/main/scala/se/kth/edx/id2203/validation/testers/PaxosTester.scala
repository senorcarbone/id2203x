package se.kth.edx.id2203.validation.testers

import java.lang.Long

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink
import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink._
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.core.SimulationPrimitives.{ProxyInterceptor, ProxyInterceptorInit, ScheduleTriggers}
import se.kth.edx.id2203.core.network.SimulationUtils
import se.kth.edx.id2203.core.{TAddress, THeader}
import se.kth.edx.id2203.validation.ValidationUtils
import se.kth.edx.id2203.validation.ValidationUtils.{Property, SimValidator, ValidationGroup, ValidationSimulation}
import se.kth.edx.id2203.validation.testers.TesterPrimitives.TesterPrimitives._
import se.sics.kompics.network.{Address, Header, Msg, Network}
import se.sics.kompics.simulator.SimulationScenario
import se.sics.kompics.simulator.network.{NetworkModel, PartitionMapper}
import se.sics.kompics.simulator.network.identifier.{Identifier, IdentifierExtractor}
import se.sics.kompics.simulator.network.identifier.impl.{SocketId, SocketIdExtractor}
import se.sics.kompics.simulator.network.impl.NetworkModels
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.sl.simulator._
import se.sics.kompics.sl.{ComponentDefinition, Init, _}
import se.sics.kompics.timer.Timer
import se.sics.kompics.{KompicsEvent, Start}

import scala.language.existentials
import scala.collection.JavaConversions._
import scala.collection.mutable._
import collection.immutable.Set
import collection.mutable.ListBuffer
import scala.collection.mutable
import scala.concurrent.duration.{DurationInt, _}
import scala.reflect.runtime.universe._
import scala.util.Random

object PaxosTester {

  val LOG_STATE: String = "simulation.delivered"

  def apply[CTYPE <: ComponentDefinition: TypeTag](): collection.Iterable[ValidationGroup] = {
    SimulationScenario.setSeed(43234)
    ValidationSimulation.runAndValidate(ConsensusValidation[CTYPE](4, 3, 30l, 80l, 1).scenario);
  }

  class InterceptorPayload(id: Int) extends KompicsEvent with Serializable;

  // - Program interfaces for Simulation

  case class BroadcastedMessage(src: Address, seq: Int) extends InterceptorPayload(seq);

  class ConsensusInterceptor(init: Init[ConsensusInterceptor]) extends ProxyInterceptor[Consensus] {

    val (self, pInit) = init match {
      case Init(s: Address, proxyInit: ProxyInterceptorInit) => (s, proxyInit)
    }

    val consensus = requires[Consensus];

    ctrl uponEvent {
      case _: Start => handle {
        initialize(pInit);
      }
    }

    consensus uponEvent {
      case x @ C_Decide(y) => handle{
        val gv = cfg.getValue[GlobalView]("simulation.globalview");
        val stateLogs = gv.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[C_Decide]]]);
        val myList = stateLogs.get(self).getOrElse(ListBuffer.empty[C_Decide]);
        myList.add(C_Decide(ValidationUtils.unfoldValue(y)));
        stateLogs.put(self, myList);
        gv.setValue(LOG_STATE, stateLogs);
      }
    }

    override def getInterceptedPort(): PositivePort[Consensus] = consensus;

    override def getSelfAddress(): Address = self
  }

  class ConsensusParent(init: Init[ConsensusParent]) extends ComponentDefinition {

    val (selfAddr, topology, paxosClass, proxyInit) = init match {
      case Init(s: Address, t: Set[Address] @unchecked, pClass: Class[ComponentDefinition] @unchecked,
        m: ProxyInterceptorInit) => (s, t, pClass, m)
    };

    val timer = requires[Timer];
    val network = requires[Network];

    val pLink = create(classOf[PerfectP2PLink], PerfectLinkInit(selfAddr));
    val beb = create(classOf[ProvidedBroadcast], Init[ProvidedBroadcast](selfAddr, topology))
    val consensus = create(paxosClass, Init[ComponentDefinition](selfAddr, topology.size));
    val interceptor = create(classOf[ConsensusInterceptor], Init[ConsensusInterceptor](selfAddr, proxyInit));

    connect[Network](network -> pLink);
    connect[Timer](timer -> interceptor);

    connect[PerfectLink](pLink -> beb);
    connect[PerfectLink](pLink -> consensus);
    connect[BestEffortBroadcast](beb -> consensus);
    connect[Consensus](consensus -> interceptor);
  }

  /**
   * Single Value Consensus
   *
   * Properties Checked
   * ------------------
   *
   * CON1: Validity: Only proposed values may be decided
   * CON2: Uniform Agreement: No two nodes decide different values
   * CON3: Integrity: Each node can decide a value at most once
   * CON4: Termination: Every node eventually decides a value
   *
   */

  case class ConsensusValidation[CTYPE <: ComponentDefinition: TypeTag](numNodes: Int, numProposers: Int, latencyMin: Long, latencyMax: Long, skew: Int) {

    import Distributions._

    assert(numNodes >= numProposers);

    class MapAddresses(partition: Set[Identifier]) extends PartitionMapper[Identifier]{
      override def getPartition(nodeId: Identifier): Int = if(partition.contains(nodeId)) 0 else 1;
    }
    
// curious to find out why this does not work...
//    
//    val splNet = (extr: IdentifierExtractor, modelIn:NetworkModel, modelOut:NetworkModel, partition: Set[Identifier]) => new NetworkModel{
//       override def getLatencyMs(message : Msg[_ <: Address, _ <: Header[_<:Address]]): Long = {
//        
//        val srcId = extr.extract(message.getHeader.asInstanceOf[THeader].getSource)
//        val dstId = extr.extract(message.getHeader.asInstanceOf[THeader].getDestination)
//
//        val srcPartition =  if(partition.contains(srcId)) 0 else 1;
//        val destPartition = if(partition.contains(dstId)) 0 else 1;
//        if (srcPartition == destPartition)
//          modelIn.getLatencyMs(message.asInstanceOf[Msg[ADR, HDR] forSome {type ADR <: Address; type HDR <: Header[ADR]}])
//        else
//          modelOut.getLatencyMs(message.asInstanceOf[Msg[ADR, HDR] forSome {type ADR <: Address; type HDR <: Header[ADR]}])
//      }
//
//    }

// new MapAddresses(topology.slice((topology.size/2)-1,topology.size).map(x => new SocketIdExtractor().extract(x)).toSet[Identifier])
    
    val splitNetwork = () => Op((_: Unit) => ChangeNetwork(
       SimulationUtils.newDisconnectedModel(new SocketIdExtractor(),
        NetworkModels.withUniformRandomDelay(latencyMin, latencyMax, SimulationScenario.getRandom),
        NetworkModels.withConstantDelay(100*latencyMax),
        topology.slice(((topology.size+1)/2),topology.size).map(x => new SocketIdExtractor().extract(x)).toSet[Identifier])
    ));

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
      StartNode(toAddress(self), Init[ConsensusParent](toAddress(self), topology,
        asJavaClass[CTYPE](typeOf[CTYPE]), ScheduleTriggers(toAddress(self))(triggers: _*)))
    });

    val prop1 =
      Property("Validity")
        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[C_Decide]]])
          .forall(_._2.forall(x => msgs.map(_.value).contains(x.value))), 109999,
          "Every value decided has been previously proposed",
          "There exist values decided that were not previously proposed")).build();

    val prop2 =
      Property("Uniform Agreement")
        .addValidator(SimValidator(gv =>
          {
            val decided = gv.getValue(LOG_STATE, classOf[mutable.HashMap[Address, ListBuffer[C_Decide]]])
            decided.isEmpty || decided.groupBy(_._2).size == 1
          }, 109999,
          "There were no two nodes deciding different values",
          "There were different decided values among different nodes")).build();

    val prop3 =
      Property("Integrity")
        .addValidator(SimValidator(_.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[C_Decide]]])
          .forall(_._2.size < 2), 109999,
          "Each node decided a value at most once",
          "There were nodes that decided more than once")).build();

    val prop4 =
      Property("Termination")
        .addValidator(SimValidator(gv =>
          {
            val decided = gv.getValue(LOG_STATE, classOf[HashMap[Address, ListBuffer[C_Decide]]]);
            topology.forall(process => {
              decided.get(process) match{
                case Some(dec) => !dec.isEmpty;
                case _ =>  false;
              }
            })
          }, 109999,
          "Each node eventually decides",
          "The decision process might not terminate")).build();


    val scenario =
      ValidationSimulation(prop1, prop2, prop3, prop4) inParallel (
        raise(1, setupOp).arrival(Distributions.constant(0l)),
        raise(1, splitNetwork()).arrival(constant(0))) andThen 10.milliseconds afterTermination
        raise(numNodes, startPaxos, 1.toN).arrival(constant(0l)) andThen 5.seconds afterTermination //let propose pass
        raise(1, setUniformLatencyNetwork(), constant(latencyMin), constant(latencyMax)).arrival(constant(0)) andThen 1000000.millis afterTermination Terminate;
        //now we should get a majority
        
    
//    val scenario =
//      ValidationSimulation(prop1, prop2, prop3, prop4) inParallel (
//        raise(1, setupOp).arrival(Distributions.constant(0l)), raise(1, setUniformLatencyNetwork(),
//          constant(latencyMin), constant(latencyMax)).arrival(constant(0)),
//          raise(numNodes, startPaxos, 1.toN).arrival(constant(0l))) andThen 1000000.millis afterTermination Terminate;
  }

}
