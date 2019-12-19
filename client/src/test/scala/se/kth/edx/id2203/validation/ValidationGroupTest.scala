package org.apache.zeppelin.kompics.validation

import org.junit.Assert._
import org.junit.Test
import org.scalatestplus.junit.AssertionsForJUnit
import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils
import se.kth.edx.id2203.validation.ValidationUtils._
import se.sics.kompics.network._
import se.sics.kompics.simulator.SimulationScenario
import se.sics.kompics.simulator.util.GlobalView
import se.sics.kompics.sl.simulator.Distributions._
import se.sics.kompics.sl.simulator.{Op, Setup, StartNode, Terminate, _}
import se.sics.kompics.sl.{ComponentDefinition, _}
import se.sics.kompics.timer.Timer
import se.sics.kompics.{Init, ComponentDefinition => _, Port => _, _}

import scala.concurrent.duration._


class ValidationGroupTest extends AssertionsForJUnit {

  @Test def singlePropertyCheck(): Unit = {

    val prop_testInitValue: ValidationGroup = Property(s"Property for ${TestHelper.token1}")
      .addValidator(SimValidator(v => v.getValue(TestHelper.token1, classOf[Int]) == 10, 10,
        s"Value of ${TestHelper.token1} is initialized correctly",
        s"Value of ${TestHelper.token1} is not initialized correctly"))
      .addValidator(SimValidator(v => v.getValue(TestHelper.token1, classOf[Int]) == 50, 30,
        s"Value of ${TestHelper.token1} is updated correctly",
        s"Value of ${TestHelper.token1} is not updated correctly")).build();

    val evalProps = ValidationSimulation.runAndValidate(
      ValidationSimulation(prop_testInitValue) inParallel
      raise(1, TestHelper.initValues(Seq((TestHelper.token1, 10)))).arrival(constant(0)) andThen 20.millis afterTermination
      raise(1, TestHelper.updateValue(TestHelper.token1, 50)).arrival(constant(0)) andThen 20.millis afterTermination Terminate);
    
    println("RESULTS \n----------- \n");
    evalProps.foreach(x => println(x.toString() + "\n"));
    //test validator result
    evalProps.foreach(x => assertTrue(x.isValid()));
      
  }
  
  @Test def singleFailedPropertyCheck(): Unit = {
    val prop_testToken1: ValidationGroup = Property(s"Property for ${TestHelper.token1}")
      .addValidator(SimValidator(v => v.getValue(TestHelper.token1, classOf[Int]) == 10, 10,
        s"Value of ${TestHelper.token1} is initialized correctly",
        s"Value of ${TestHelper.token1} is not initialized correctly"))
      .addValidator(SimValidator(v => v.getValue(TestHelper.token1, classOf[Int]) == 10, 30,
        s"Value of ${TestHelper.token1} is updated correctly",
        s"Value of ${TestHelper.token1} is not the expected one")).build();

    val evalProps = ValidationSimulation.runAndValidate(
      ValidationSimulation(prop_testToken1) inParallel
        raise(1, TestHelper.initValues(Seq((TestHelper.token1, 10)))).arrival(constant(0)) andThen 20.millis afterTermination
        raise(1, TestHelper.updateValue(TestHelper.token1, 50)).arrival(constant(0)) andThen 20.millis afterTermination Terminate);

    println("RESULTS \n----------- \n");
    evalProps.foreach(x => println(x.toString() + "\n"));
    //test validator result
    assertFalse(evalProps.head.isValid());
  }

  @Test def multiPropertyCheck(): Unit = {
             
    val prop_testToken1: ValidationGroup = Property(s"Property for ${TestHelper.token1}")
      .addValidator(SimValidator(v => v.getValue(TestHelper.token1, classOf[Int]) == 10, 10,
        s"Value of ${TestHelper.token1} is initialized correctly",
        s"Value of ${TestHelper.token1} is not initialized correctly"))
      .addValidator(SimValidator(v => v.getValue(TestHelper.token1, classOf[Int]) == 50, 30,
        s"Value of ${TestHelper.token1} is updated correctly",
        s"Value of ${TestHelper.token1} is not the expected one")).build();
    
    val prop_testToken2: ValidationGroup = Property(s"Property for ${TestHelper.token2}")
      .addValidator(SimValidator(v => v.getValue(TestHelper.token2, classOf[Int]) == 10, 10,
        s"Value of ${TestHelper.token2} is initialized correctly",
        s"Value of ${TestHelper.token2} is not initialized correctly"))
      .addValidator(SimValidator(v => v.getValue(TestHelper.token2, classOf[Int]) == 60, 50,
        s"Value of ${TestHelper.token2} is updated correctly",
        s"Value of ${TestHelper.token2} is not the expected one")).build();

    val evalProps = ValidationSimulation.runAndValidate(
      ValidationSimulation(prop_testToken1, prop_testToken2) inParallel
        raise(1, TestHelper.initValues(Seq((TestHelper.token1, 10),(TestHelper.token2, 10)))).arrival(constant(0)) andThen 20.millis afterTermination
        raise(1, TestHelper.updateValue(TestHelper.token1, 50)).arrival(constant(0)) andThen 20.millis afterTermination
        raise(1, TestHelper.updateValue(TestHelper.token2, 60)).arrival(constant(0)) andThen 10.millis afterTermination Terminate);

    println("RESULTS \n----------- \n");
    evalProps.foreach(x => println(x.toString() + "\n"));
    evalProps.foreach(x => assertTrue(x.isValid()))

  }
}


object TestHelper {

  val token1 = "sim.token1";
  val token2 = "sim.token2";

  val initValues = (initVals : Seq[(String, Int)]) => Op((_: Unit) => Setup((gv: GlobalView) => initVals.foreach(tpl => gv.setValue(tpl._1,tpl._2))).build());
  val updateValue = (token: String, updated: Int) => Op((_: Unit) => StartNode(AddressUtils.toAddress(SimulationScenario.getRandom.nextInt(255)), TestInjectorInit(token, updated)));
  
  case class TestInjectorInit(token: String, value: Int) extends Init[TestInjector];

  class TestInjector(init: TestInjectorInit) extends ComponentDefinition {
    val network = requires[Network];
    val timer = requires[Timer];

    def getGlobalView(): GlobalView = {
      cfg.getValue[GlobalView]("simulation.globalview")
    }

    ctrl uponEvent {
      case _: Start => {
        getGlobalView().setValue(init.token, init.value);
      }
    }
  }

}
