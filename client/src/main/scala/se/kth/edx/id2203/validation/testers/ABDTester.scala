package se.kth.edx.id2203.validation.testers

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.ExercisePrimitives.PerfectP2PLink
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
import se.sics.kompics.timer.{ScheduleTimeout, Timeout, Timer}
import se.sics.kompics.{KompicsEvent, Start}

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.reflect.runtime.universe._
import scala.util.Random


object ABDTester {

  val GLOBAL_STATE: String = "simulation.delivered"

  def apply[ARTYPE <: ComponentDefinition : TypeTag](): collection.Iterable[ValidationGroup] = {
    val seed = 1337;
    SimulationScenario.setSeed(seed);
    ValidationSimulation.runAndValidate(NNRegisterValidation[ARTYPE](8, 4, 50l, 100, 50, 90).scenario);
  }

  class InterceptorPayload(id: Int) extends KompicsEvent with Serializable;

  // - Program interfaces for Simulation

  case class BroadcastedMessage(src: Address, seq: Int) extends InterceptorPayload(seq);

  class NNARInterceptor(init: Init[NNARInterceptor]) extends ProxyInterceptor[AtomicRegister] {

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
      case x: AR_Read_Response => handle {
        val gv = cfg.getValue[GlobalView]("simulation.globalview");
        val globalState = gv.getValue(GLOBAL_STATE, classOf[ListBuffer[(Address, Long, KompicsEvent)]]);
//        println(s"${System.currentTimeMillis()} -- INTERCEPTOR :: Reader ${toRank(self)} FINISHED $x")
        globalState.add((self, System.currentTimeMillis(), x));
        gv.setValue(GLOBAL_STATE, globalState);
      }
      case x: AR_Write_Response => handle {
        val gv = cfg.getValue[GlobalView]("simulation.globalview");
        val globalState = gv.getValue(GLOBAL_STATE, classOf[ListBuffer[(Address, Long, KompicsEvent)]]);
//        println(s"${System.currentTimeMillis()} -- INTERCEPTOR :: Writer ${toRank(self)} FINISHED $x")
        globalState.add((self, System.currentTimeMillis(), x));
        gv.setValue(GLOBAL_STATE, globalState);
      }
    }

    override def getInterceptedPort(): PositivePort[AtomicRegister] = nnar

    override def getSelfAddress(): Address = self
  }

  class NNARParent(init: Init[NNARParent]) extends ComponentDefinition {

    val (selfAddr, topology, nnarClass, proxyInit) = init match {
      case Init(s: Address, t: collection.immutable.Set[Address]@unchecked, nnarClass: Class[ComponentDefinition]@unchecked,
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
  
  def extractOps(events: List[(Address, Long, KompicsEvent)]): List[AR_Op] = {
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

    val allOps: List[AR_Op] = extractOps(triggeredEvents.toList);

    val withReadDependencies = allOps.map(evt => {
      if (!evt.write && evt.value != None) {
        val writeMatches = allOps.filter(writeop => writeop.write && writeop.value == evt.value)
        if (!writeMatches.isEmpty) AR_Op(false, evt.value, evt.start, evt.stop, evt.edges + writeMatches.get(0)) else evt
      }
      else evt;
    });

    val withOrderDependencies = withReadDependencies.map(evt => {
      val succ = withReadDependencies.filter(evt2 => evt.start > evt2.stop);
      AR_Op(evt.write, evt.value, evt.start, evt.stop, evt.edges++succ);
    }).toSet

//    println(withOrderDependencies)

    checkLinearizability(Set(Linerization(withOrderDependencies, None)));
  }

  def canAppend(op1: AR_Op, op2: AR_Op): Boolean = op2.write || (!op2.write && op2.value == op1.value)

  def extendLinearizations(liner: (Linerization, Set[AR_Op])): Set[Linerization] =
    liner._2.filter(liner._1.tail == None || canAppend(liner._1.tail.get, _)).map(validOp => {
      Linerization((liner._1.pend - validOp).map(p => AR_Op(p.write, p.value, p.start, p.stop, p.edges - validOp)), Some(validOp));
    });

  case class Linerization(pend: Set[AR_Op], tail: Option[AR_Op]);

  @tailrec
  final def checkLinearizability(nodes: Set[Linerization]): Boolean = {

    //find next and filter out branches with cycles
    val extended = nodes.map(node => (node, node.pend.filter(_.edges.isEmpty)))
      .filter(lin => !(!lin._1.pend.isEmpty && lin._2.isEmpty));

    //if no linearizable pending branches exist then it's a dead-end
    if (extended.isEmpty) {
      return false;
    }
    else if (extended.exists(branch => branch._1.pend.isEmpty)) {
      return true;
    }
    //expand linearizations
    val expanded = extended.flatMap(nxt => extendLinearizations(nxt._1, nxt._2));
    //    if (expanded.exists(branch => branch.pend.isEmpty)){
    //      return true;
    //    }
    checkLinearizability(expanded);
  }

  /**
    * (N,N) Atomic Register
    *
    * Properties Checked
    * ------------------ 
    *
    * NNAR1: Termination: If a correct process invokes an operation, then the operation eventually completes.
    * NNAR2: Atomicity: Every read operation returns the value that was written most recently in a hypothetical execution, 
    * where every failed operation appears to be complete or does not appear to have been invoked at all, 
    * and every complete operation appears to have been executed at some instant between its invocation and its completion.
    *
    */

  case class AR_Op(write: Boolean, value: Option[Any], start: Long, stop: Long, edges: Set[AR_Op]);


  case class NNRegisterValidation[ARTYPE <: ComponentDefinition : TypeTag](numNodes: Int, numWriters: Int, netFixedLatency: Long, freq: Int, writeSkew: Int, readSkew: Int) {

    import Distributions._

    //this is a true random (not using the simulator seed) to ensure uniqueness across validations
    val instantRandom = new Random();
    val msgs = Seq.fill(numWriters)(AR_Write_Request(instantRandom.nextInt())).toArray
    val topology = (1 to numNodes).map(toAddress(_)).toSet;

    val setConstantLatencyNetwork = () => Op((maxDelay: java.lang.Long) => ChangeNetwork(NetworkModels.withConstantDelay(maxDelay)));

    //Simulation Operations ----
    val setupOp = Op((_: Unit) => Setup((gv: GlobalView) => gv.setValue(GLOBAL_STATE, ListBuffer.empty[(Address, Long, KompicsEvent)])).build());

    val writeTriggers = (1 to numWriters).map(x => (toAddress(x), Int.box((x * freq) + writeSkew).toLong, msgs(x - 1)))
    val readTriggers = (numWriters + 1 to numNodes).map(x => (toAddress(x), Int.box(((x - numWriters) * freq) + readSkew).toLong, AR_Read_Request()))

    val startNNAR = Op((self: Integer) => StartNode(toAddress(self), Init[NNARParent](toAddress(self), topology,
      asJavaClass[ARTYPE](typeOf[ARTYPE]), ScheduleTriggers(toAddress(self))((writeTriggers ++ readTriggers).toArray: _*))));

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