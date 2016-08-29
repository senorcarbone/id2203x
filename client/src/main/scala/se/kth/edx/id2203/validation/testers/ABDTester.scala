package se.kth.edx.id2203.validation.testers

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink
import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink._
import se.kth.edx.id2203.core.Ports._
import se.kth.edx.id2203.core.SimulationPrimitives.{ProxyInterceptor, ProxyInterceptorInit, ScheduleTriggers}
import se.kth.edx.id2203.validation.ValidationUtils.{Property, SimValidator, ValidationGroup, ValidationSimulation}
import se.kth.edx.id2203.validation.testers.TesterPrimitives.TesterPrimitives._
import se.sics.kompics.{KompicsEvent, Start}
import se.sics.kompics.network.{Address, Network}
import se.sics.kompics.simulator.network.impl.NetworkModels
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.simulator.SimulationScenario
import se.sics.kompics.sl.simulator._
import se.sics.kompics.sl.{ComponentDefinition, Init, _}
import se.sics.kompics.timer.{ScheduleTimeout, Timeout, Timer}

import scala.collection.JavaConversions._
import collection.mutable.ListBuffer
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.Random


object ABDTester {

  val GLOBAL_STATE: String = "simulation.delivered"

  def apply[ARTYPE <: ComponentDefinition : TypeTag](): collection.Iterable[ValidationGroup] = {
    val seed = 123;
    SimulationScenario.setSeed(seed);
    ValidationSimulation.runAndValidate(NNRegisterValidation[ARTYPE](20, 10, 5l, 5, 10, 5).scenario);
  }

  class InterceptorPayload(id: Int) extends KompicsEvent with Serializable;

  // - Program interfaces for Simulation

  case class BroadcastedMessage(src: Address, seq: Int) extends InterceptorPayload(seq);

  class NNARInterceptor(init : Init[NNARInterceptor]) extends ProxyInterceptor[AtomicRegister]{

    val (self, pInit) = init match {
      case Init(s: Address, proxyInit: ProxyInterceptorInit) => (s, proxyInit)
    }

    case class RegisterOperationStart(timeout: ScheduleTimeout, event: KompicsEvent) extends Timeout(timeout);
    
    val nnar = requires[AtomicRegister];

    def scheduleWriteReqLogging(triggerEvent: (Long, KompicsEvent)) = {
      val timeout = new ScheduleTimeout(triggerEvent._1);
      timeout.setTimeoutEvent(RegisterOperationStart(timeout, triggerEvent._2));
      trigger(timeout -> timer);
    }
    
    ctrl uponEvent {
      case _: Start => handle {
        initialize(pInit);
//        pInit.triggers.filter(_._2.isInstanceOf[AR_Write_Request]).foreach(scheduleWriteReqLogging(_));
        pInit.triggers.foreach(scheduleWriteReqLogging(_));
      }
    }

    timer uponEvent {
      case RegisterOperationStart(_, writeEvent) => handle {
        val gv = cfg.getValue[GlobalView]("simulation.globalview");
        val globalState = gv.getValue(GLOBAL_STATE, classOf[ListBuffer[(Address, Long, KompicsEvent)]]);
        globalState.add((self, System.currentTimeMillis(), writeEvent))
      }
    }

    nnar uponEvent {
      case x: AR_Read_Response => handle{
        val gv = cfg.getValue[GlobalView]("simulation.globalview");
        val globalState = gv.getValue(GLOBAL_STATE, classOf[ListBuffer[(Address, Long, KompicsEvent)]]);
        println(s"INTERCEPTOR :: Reader ${toRank(self)} FINISHED $x")
        globalState.add((self, System.currentTimeMillis(), x));
        gv.setValue(GLOBAL_STATE, globalState);
      }
      case x: AR_Write_Response => handle{
        val gv = cfg.getValue[GlobalView]("simulation.globalview");
        val globalState = gv.getValue(GLOBAL_STATE, classOf[ListBuffer[(Address, Long, KompicsEvent)]]);
        println(s"INTERCEPTOR :: Writer ${toRank(self)} FINISHED $x")
        globalState.add((self, System.currentTimeMillis(), x));
        gv.setValue(GLOBAL_STATE, globalState);
      }
    }

    override def getInterceptedPort(): PositivePort[AtomicRegister] = nnar

    override def getSelfAddress(): Address = self
  }
  
  class NNARParent(init: Init[NNARParent]) extends ComponentDefinition {

    val (selfAddr, topology, nnarClass, proxyInit) = init match {
      case Init(s: Address, t: collection.immutable.Set[Address] @unchecked, nnarClass: Class[ComponentDefinition] @unchecked,
      m: ProxyInterceptorInit) => (s, t, nnarClass, m)
    };

    val timer = requires[Timer];
    val network = requires[Network];

    val pp2p = create(classOf[PerfectP2PLink], PerfectLinkInit(selfAddr));
    val beb = create(classOf[ProvidedBroadcast], Init[ProvidedBroadcast](selfAddr, topology))
    val nnar = create(nnarClass, Init[ComponentDefinition](selfAddr, topology.size));
    val interceptor = create(classOf[NNARInterceptor], Init[NNARInterceptor](selfAddr, proxyInit));

    connect[Network](network -> pp2p);
    connect[Timer](timer -> interceptor);

    connect[PerfectLink](pp2p -> beb);
    connect[PerfectLink](pp2p -> nnar);
    connect[BestEffortBroadcast](beb -> nnar);
    connect[AtomicRegister](nnar -> interceptor);
  }

  /**
    * (N,N) Atomic Register
    *
    * Properties Checked
    * ------------------ 
    * 
    *  NNAR1: Termination: If a correct process invokes an operation, then the operation eventually completes.
    *  NNAR2: Atomicity: Every read operation returns the value that was written most recently in a hypothetical execution, 
    *  where every failed operation appears to be complete or does not appear to have been invoked at all, 
    *  and every complete operation appears to have been executed at some instant between its invocation and its completion.
    *  
    */

  case class AR_Op (write: Boolean, value: Option[Any], start: Long, stop: Long, seqDependencies: Set[AR_Op]);


  case class NNRegisterValidation[ARTYPE <: ComponentDefinition : TypeTag](numNodes: Int, numWriters: Int, netFixedLatency: Long, freq: Int, writeSkew: Int,  readSkew: Int) {

    import Distributions._

    //this is a true random (not using the simulator seed) to ensure uniqueness across validations
    val instantRandom = new Random();
    val msgs = Seq.fill(numWriters)(AR_Write_Request(instantRandom.nextInt())).toArray
    val topology = (1 to numNodes).map(toAddress(_)).toSet;

    val setConstantLatencyNetwork = () => Op((maxDelay: java.lang.Long) => ChangeNetwork(NetworkModels.withConstantDelay(maxDelay)));
    
    //Simulation Operations ----
    val setupOp = Op((_: Unit) => Setup((gv: GlobalView) => gv.setValue(GLOBAL_STATE, ListBuffer.empty[(Address, Long, KompicsEvent)])).build());

    val writeTriggers = (1 to numWriters).map(x => (toAddress(x), Int.box((x * freq) + writeSkew).toLong, msgs(x-1)))
    val readTriggers = (numWriters+1 to numNodes).map(x => (toAddress(x), Int.box(((x-numWriters) * freq) + readSkew).toLong, AR_Read_Request()))
    
    val startNNAR = Op((self: Integer) => StartNode(toAddress(self), Init[NNARParent](toAddress(self), topology,
      asJavaClass[ARTYPE](typeOf[ARTYPE]), ScheduleTriggers(toAddress(self))((writeTriggers ++ readTriggers).toArray:_*))));

    def extractOps(events: ListBuffer[(Address, Long, KompicsEvent)]): ListBuffer[AR_Op] = {
      events.filter(_._3.isInstanceOf[AR_Read_Request]).zip(events.filter(_._3.isInstanceOf[AR_Read_Response])).map(matches => {
        val req = matches._1.asInstanceOf[(Address, Long, AR_Read_Request)];
        val resp = matches._2.asInstanceOf[(Address, Long, AR_Read_Response)];
        AR_Op(false, resp._3.value, req._2, resp._2, Set.empty[AR_Op]);
      }) ++ events.filter(_._3.isInstanceOf[AR_Write_Request]).zip(events.filter(_._3.isInstanceOf[AR_Write_Response])).map(matches => {
          val req = matches._1.asInstanceOf[(Address, Long, AR_Write_Request)];
          val resp = matches._2.asInstanceOf[(Address, Long, AR_Write_Response)];
          AR_Op(true, Some(req._3.value), req._2, resp._2, Set.empty[AR_Op]);
        })
    };

    def validateLinerizability(triggeredEvents: ListBuffer[(Address, Long, KompicsEvent)]): Boolean = {
      
      val allOps : ListBuffer[AR_Op] = extractOps(triggeredEvents)
        
      val processedReads = allOps.map(evt => {
        if(!evt.write && evt.value != None){
          val writeMatch = allOps.filter(writeop => writeop.write && writeop.value == evt.value).get(0);
          AR_Op(false, evt.value, evt.start, evt.stop, evt.seqDependencies+writeMatch)
        }
        else evt;
      });
      
      val consDependencies = processedReads.map(evt =>{
        val succ = processedReads.filter(evt2 => evt.start > evt2.stop);
        succ.foldLeft(evt)((myevt, next) => AR_Op(evt.write, evt.value, evt.start, evt.stop, evt.seqDependencies+next));
      })
      
      liner_check(consDependencies, Set.empty[List[AR_Op]]);
    }

    def liner_execution(execution : List[AR_Op]): Boolean = {
      execution.foldLeft[Option[Any]](None)((b, nextOp) => {
        if (nextOp.write) nextOp.value
        else {
          if (nextOp.value != b) {
            return false;
          }
          b
        }
      });
      return true;
    }
    
    @tailrec
    final def liner_check(remainingOps : ListBuffer[AR_Op], executions : collection.immutable.Set[List[AR_Op]]) : Boolean = {

      //leaf: check atomicity violations in executions
      if (remainingOps.isEmpty) {
        return executions.exists(liner_execution)
      }
      
      //continue traversal
      val toTraverse = remainingOps.filter(_.seqDependencies.isEmpty).toSet[AR_Op];
      val filteredRemaining = remainingOps.filter(x => !toTraverse.contains(x)).map(y => {
        AR_Op(y.write, y.value, y.start, y.stop, y.seqDependencies -- y.seqDependencies.intersect(toTraverse))
      });
        
      val additionalExecutions = if(!executions.isEmpty) toTraverse.flatMap(x => { executions.map(ex => ex :+ x)})
      else toTraverse.map(List(_));
      liner_check(filteredRemaining, executions ++ additionalExecutions);
    }

    def terminationCheck(triggeredEvents: ListBuffer[(Address, Long, KompicsEvent)]): Boolean = {
      topology.forall(addr => triggeredEvents.exists(entry => entry._3 match {
        case AR_Read_Response(value) if entry._1 == addr => true;
        case AR_Write_Response() if entry._1 == addr => true;
        case _ => false;
      }));
    }
    
    val prop1 =
      Property("Termination")
        .addValidator(SimValidator(x => terminationCheck(x.getValue(GLOBAL_STATE, classOf[ListBuffer[(Address, Long, KompicsEvent)]])), 109999,
          "Every operation invoked by a correct process eventually completes",
          "There exist operations invoked by a correct process that never complete")).build();

    val prop2 =
      Property("Atomicity/Linerizability")
        .addValidator(SimValidator(x => validateLinerizability(x.getValue(GLOBAL_STATE, classOf[ListBuffer[(Address, Long, KompicsEvent)]])), 109999, 
          "Every read operation returns the value that was written most recently in a hypothetical global execution",
          "There is no hypothetical global execution where all read operations can return the value that was written most recently")).build();
    
    val scenario =
      ValidationSimulation(prop1, prop2) inParallel(
        raise(1, setupOp).arrival(Distributions.constant(0l)), 
        raise(1, setConstantLatencyNetwork(), constant(netFixedLatency)).arrival(constant(0)),
        raise(numNodes, startNNAR, 1.toN).arrival(constant(0l))) andThen 1000000.millis afterTermination Terminate;
  }

}