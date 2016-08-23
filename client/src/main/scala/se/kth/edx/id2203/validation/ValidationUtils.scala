package se.kth.edx.id2203.validation

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils
import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import se.sics.kompics.network.Network
import se.sics.kompics.timer.{ScheduleTimeout, Timeout}
import se.sics.kompics.Start
import se.sics.kompics.sl._
import se.sics.kompics.sl.simulator._
import se.sics.kompics.simulator.SimulationScenario
import se.sics.kompics.simulator.run.LauncherComp
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.sl.ComponentDefinition
import se.sics.kompics.sl.simulator.{Op, StartNode}
import se.sics.kompics.{Init, ComponentDefinition => _, Port => _, _}
import se.sics.kompics.timer.Timer
import Distributions._

import scala.collection.mutable

import spray.json._
import spray.json.DefaultJsonProtocol._


 
object ValidationUtils {

  case class PropertyObserverInit(properties: Iterable[ValidationGroup], terminateOnInvalid: Boolean = false) extends Init[PropertyObserver];

  case class ValidationResult(valid: Boolean, msg: String){
    override def toString(): String = (if (valid) "✔: " else "✘: ") + msg; 
  }

  case class SimValidator(predicate: GlobalView => Boolean, triggerTime: Long, successMsg: String = "", 
                          failureMsg: String = "") {
    var passed = false;
    var triggered = false;
    
    def validate(currentView: GlobalView): ValidationResult = {
      if(!triggered) {
        passed = predicate(currentView);
        triggered = true;
      }
      getResult()
    }
    
    def getResult() : ValidationResult = {
      if(triggered){
        ValidationResult(passed, if(passed) successMsg else failureMsg);
      }
      else{
        ValidationResult(false, "Validation was never triggered")
      }
    }
  }
  
  case class PropertyResult(id: String, valid: Boolean);
  
  case class ValidationGroup(id: String, validators: Iterable[SimValidator]) {

    def isValid(): Boolean = !validators.exists(x => !x.getResult().valid);
    
    override def toString(): String = {
      val padding = "   ";
      val str1 = s"-> $id :: ${if (isValid()) "SATISFIED" else "VIOLATED"} \n";
      var str2 = "";
      validators.foreach(x => str2 = str2 + padding + x.getResult() + "\n");
      return str1 + str2;
    }
  }

  case class ValidationReport(id: String, valid: Boolean, properties: Iterable[PropertyResult], points: Int = 1);

  object Property {

    class PropertyBuilder {

      private var validators: mutable.Set[SimValidator] = mutable.Set[SimValidator]();
      private var description: String = "Anonymous Property";

      def addValidator(validator: SimValidator): PropertyBuilder = {
        validators += validator;
        PropertyBuilder.this
      }

      def withDescription(description: String): PropertyBuilder = {
        this.description = description;
        PropertyBuilder.this
      }

      def build(): ValidationGroup = {
        ValidationGroup(description, validators)
      }
    }

    def apply(): PropertyBuilder = {
      new PropertyBuilder();
    }

    def apply(description: String) = {
      val builder = new PropertyBuilder();
      builder.withDescription(description)
    }

    def apply(validators: SimValidator*) = {
      val builder = new PropertyBuilder();
      validators.foreach(builder.addValidator(_));
      builder
    }
  }

  object ValidationSimulation {

    implicit val propsFormat = jsonFormat2(PropertyResult)
    implicit val reportFormat = jsonFormat4(ValidationReport)

    def apply(props: ValidationGroup*) = {
      val startValidator = Op((_:Unit) => StartNode(AddressUtils.toAddress(0), PropertyObserverInit(props.toSet)));
      raise(1, startValidator).arrival(constant(0))
    }
    
    def runAndValidate(sc: SimulationScenario): Iterable[ValidationGroup] = {
      sc.simulate(classOf[LauncherComp]);
      val ois = new ObjectInputStream((new FileInputStream(("/tmp/observer"))));
      val properties =  ois.readObject().asInstanceOf[scala.collection.immutable.Set[ValidationGroup]];
      ois.close();
      return properties;
    }

  }

  class PropertyObserver(init: PropertyObserverInit) extends ComponentDefinition {

    val properties: Set[ValidationGroup] = init.properties.toSet[ValidationGroup];
    val timer = requires[Timer];
    val network = requires[Network];
    val terminateOnInvalid = init.terminateOnInvalid;

    var token = 0;
    var pendingValidations = mutable.Map[Int, SimValidator]();

    case class PropertyCheck(timeout: ScheduleTimeout, token: Int) extends Timeout(timeout);

    def register(validator: SimValidator): Int = {
      token = token + 1;
      println(s"PropertyObserver Registering Validator $token");
      pendingValidations.put(token, validator);
      token;
    }

    def unregister(token: Int): Option[SimValidator] = {
      println(s"PropertyObserver Unregistering Validator $token");
      pendingValidations.remove(token);
    }

    def finalizeExecution() = {
      val oos = new ObjectOutputStream(new FileOutputStream("/tmp/observer"));
      oos.writeObject(properties);
      oos.flush();
      oos.close
      getGlobalView().terminate();
    }

    def getGlobalView(): GlobalView = {
      cfg.getValue[GlobalView]("simulation.globalview")
    }

    def genTimeout(validator: SimValidator): ScheduleTimeout = {
      val timeoutEvent = new ScheduleTimeout(validator.triggerTime);
      timeoutEvent.setTimeoutEvent(PropertyCheck(timeoutEvent, register(validator)));
      timeoutEvent
    }

    ctrl uponEvent {
      case _: Start => handle {
        println("PropertyObserver Started");
        properties.foreach(_.validators.foreach(x => trigger(genTimeout(x) -> timer)));
      }
    }

    timer uponEvent {
      case PropertyCheck(_, token) => handle {
        println(s"PropertyObserver Executing Validator $token");
        val passed = unregister(token).get.validate(getGlobalView()).valid;
        if ((terminateOnInvalid && !passed) || pendingValidations.isEmpty) finalizeExecution();
      }
    }

  }

}
