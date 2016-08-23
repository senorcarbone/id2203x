package se.kth.edx.id2203

import se.sics.kompics.sl.{ComponentDefinition, PositivePort}
import se.sics.kompics.timer.{ScheduleTimeout, Timeout, Timer}
import se.sics.kompics.{KompicsEvent, PortType, Start}
import se.sics.kompics.sl._

object SimulationPrimitives {
  
  case class ProxyInterceptorInit(triggers: Iterable[(Long, KompicsEvent)]); 

  trait ProxyInterceptor[P <: PortType] extends ComponentDefinition {

    case class TriggerTimeout(timeout: ScheduleTimeout, event: KompicsEvent) extends Timeout(timeout)

    val timer = requires[Timer];

    def getInterceptedPort(): PositivePort[P]

    def initialize(init: ProxyInterceptorInit) = {
      init.triggers.foreach(scheduleTrigger(_));
    }

    def scheduleTrigger(triggerEvent: (Long, KompicsEvent)) = {
      val timeout = new ScheduleTimeout(triggerEvent._1);
      timeout.setTimeoutEvent(TriggerTimeout(timeout, triggerEvent._2));
      trigger(timeout -> timer);
    }
    
    timer uponEvent {
      case TriggerTimeout(_, evt) => handle {
        trigger(evt -> getInterceptedPort());
      }
    }

  }

  object ScheduleTriggers{
    def apply(tr: (Long, KompicsEvent)*) : ProxyInterceptorInit = ProxyInterceptorInit(tr.toList)
  }

}
