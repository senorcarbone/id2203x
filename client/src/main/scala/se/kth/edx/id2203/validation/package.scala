package se.kth.edx.id2203.validation

import se.kth.edx.id2203.validation.ValidationUtils.ValidationSimulation._
import se.kth.edx.id2203.validation.testers.EPFDTester
import se.sics.kompics.sl._
import se.sics.kompics.{ComponentDefinition => _, Port => _}
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.reflect.runtime.universe._


object validation {
  
  
  val startframeMsg = " * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * \n" +
                 " -----------------------VALIDATION RESULTS----------------------------- \n";
  
  val endframeMsg =  " * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * \n"
    
  val successMsg = (id: String) => 
    s"Congratulations! Your implementation of the '$id' satisfies all properties! " +
    s"\nA unique token has been generated for your submission right below. Please do not edit. \n";
  
  val errorMsg = (id: String) =>
    s"Unfortunately your implementation of the '$id' does not satisfy all properties. " + s"" + 
      s"\nPlease make sure your implementation covers all guarantees and try again. "
  
  val finalMsg = "Final Comments\n" +
                 "--------------";  

  case class GradingToken(gradingToken: Array[Byte]);

  implicit val gradingFormat = jsonFormat1(GradingToken)
  
  
  def checkEPFD[T <: ComponentDefinition : TypeTag](): Unit = {
    val componentId = "Eventually Perfect Failure Detector";
    val props = EPFDTester[T]();
    val passed = !props.exists(!_.isValid());
    println(startframeMsg);
    props.foreach(println(_));
    println(finalMsg)
    if (!passed) {
      println(successMsg(componentId));
      println(GradingToken(Encryptor.encrypt(Grading.gradeEPFD(props).toJson.toString)).toJson.toString);
    }
    else{
      println(errorMsg(componentId));
    }
    println(endframeMsg);
  }

}







