package se.kth.edx.id2203.validation

import java.io._

import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils
import se.sics.kompics.network.Network
import se.sics.kompics.simulator.SimulationScenario
import se.sics.kompics.simulator.run.LauncherComp
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.sl.{ ComponentDefinition, _ }
import se.sics.kompics.sl.simulator.Distributions._
import se.sics.kompics.sl.simulator.{ Op, StartNode, _ }
import se.sics.kompics.timer.{ ScheduleTimeout, Timeout, Timer }
import se.sics.kompics.{ Init, Start, ComponentDefinition => _, Port => _ }
import spray.json.DefaultJsonProtocol._

import scala.annotation.tailrec
import scala.collection.mutable

object ValidationUtils {

    case class PropertyObserverInit(properties: Iterable[ValidationGroup], terminateOnInvalid: Boolean = false) extends Init[PropertyObserver];

    case class ValidationResult(valid: Boolean, msg: String) {
        override def toString(): String = (if (valid) "✔: " else "✘: ") + msg;
    }

    case class SimValidator(predicate: GlobalView => Boolean, triggerTime: Long, successMsg: String = "",
                            failureMsg: String = "", var passed: Boolean = false, var triggered: Boolean = false) {

        def validate(currentView: GlobalView): ValidationResult = {
            if (!triggered) {
                passed = predicate(currentView);
                triggered = true;
            }
            getResult()
        }

        def getResult(): ValidationResult = {
            if (triggered) {
                ValidationResult(passed, if (passed) successMsg else failureMsg);
            } else {
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

    case class ValidationReport(id: String, valid: Boolean, properties: Iterable[PropertyResult], points: Int = 1) {
        def toMessage(): String = {
            val sb = new StringBuilder();
            sb.append("For component ");
            sb.append(id);
            sb.append(": ");
            val props = properties.map { x => x.id ++ ": " ++ (if (x.valid) { "passed" } else { "failed" }) }.mkString(", ");
            sb.append(props);
            sb.toString()
        }
    }

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
            val startValidator = Op((_: Unit) => StartNode(AddressUtils.toAddress(0), PropertyObserverInit(props.toSet)));
            raise(1, startValidator).arrival(constant(0))
        }

        def runAndValidate(sc: SimulationScenario): Iterable[ValidationGroup] = {
            val simulationFile: File = new File("/tmp/observer")
            if (simulationFile.exists()) {
                simulationFile.delete();
            }
            sc.simulate(classOf[LauncherComp]);
            val tcxtl = Thread.currentThread().getContextClassLoader();
            val ois = new ClassLoaderObjectInputStream(tcxtl, new FileInputStream("/tmp/observer"));
            val properties = ois.readObject().asInstanceOf[scala.collection.immutable.Set[ValidationGroup]];
            ois.close();
            return properties
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
            pendingValidations.put(token, validator);
            token;
        }

        def unregister(token: Int): Option[SimValidator] = {
            pendingValidations.remove(token);
        }

        def finalizeExecution() = {
            val result = properties.map(vg => ValidationGroup(vg.id, vg.validators.map(v =>
                SimValidator(null, v.triggerTime, v.successMsg, v.failureMsg, v.passed, v.triggered))));

            val oos = new ObjectOutputStream(new FileOutputStream("/tmp/observer"));
            oos.writeObject(result);
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
                properties.foreach(_.validators.foreach(x => trigger(genTimeout(x) -> timer)));
            }
        }

        timer uponEvent {
            case PropertyCheck(_, token) => handle {
                val passed = unregister(token).get.validate(getGlobalView()).valid;
                if ((terminateOnInvalid && !passed) || pendingValidations.isEmpty) finalizeExecution();
            }
        }

    }

    @tailrec
    def unfoldValue(value: Any): Any = {
        value match {
            case Some(x) => unfoldValue(x);
            case x       => x;
        }
    }

    class ClassLoaderObjectInputStream(classLoader: ClassLoader, in: InputStream) extends ObjectInputStream(in) {

        @throws(classOf[IOException])
        @throws(classOf[ClassNotFoundException])
        override protected def resolveClass(desc: ObjectStreamClass): Class[_] = {
            try {
                val name = desc.getName();
                return Class.forName(name, false, classLoader);
            } catch {
                case e: ClassNotFoundException => return super.resolveClass(desc);
            }
        }
    }

}