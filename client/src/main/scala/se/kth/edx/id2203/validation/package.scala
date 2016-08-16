package se.kth.edx.id2203.validation

import ValidationUtils.ValidationSimulation._
import scala.reflect.runtime.universe._
import se.kth.edx.id2203.validation.testers.EPFDTester
import se.sics.kompics.sl._
import se.sics.kompics.{ComponentDefinition => _, Port => _, _}
import spray.json._
import DefaultJsonProtocol._


object validation {
  
  case class GradingToken(token: Array[Byte]);
  implicit val gradingFormat = jsonFormat1(GradingToken)
  
  def checkEPFD[T <: ComponentDefinition : TypeTag]() : Unit = {
      val props =  EPFDTester[T]();
      val passed = !props.exists(!_.isValid());
      println(props.toString());
//      if(passed){
        println("Congratulations! You have passed this part. A unique token has been generated for your submission. Please do not edit.");
        println(GradingToken(Encryptor.encrypt(Grading.gradeEPFD(props).toJson.toString)).toJson.toString);
//      }
    
  }
  
}







