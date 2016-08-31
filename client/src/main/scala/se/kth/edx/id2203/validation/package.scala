package se.kth.edx.id2203

import java.io.{File, FileOutputStream, PrintStream}

import se.kth.edx.id2203.validation.ValidationUtils.{PropertyResult, ValidationGroup, ValidationReport}
import se.kth.edx.id2203.validation.ValidationUtils.ValidationSimulation._
import se.kth.edx.id2203.validation.testers._
import se.sics.kompics.sl._
import se.sics.kompics.{ComponentDefinition => _, Port => _}
import spray.json.DefaultJsonProtocol._
import spray.json._

import scala.reflect.runtime.universe._


package object validation {
  
  val outputLinkMsg = "\nYour submission has been locally simulated and validated.\n" +
    "\n<a href=\"file:////tmp/output.txt\">Click Here</a> to view the output of the simulation \n "
  
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
  
  var defaultOutput : PrintStream = System.out;

  case class GradingToken(gradingToken: Array[Byte]);

  implicit val gradingFormat = jsonFormat1(GradingToken)

  def outToFile(): Unit = {
    defaultOutput = System.out; // save current endpoint
    System.out.flush();
    Console.out.flush();
    val outFile: File = new File("/tmp/output")
    if (outFile.exists()) {
      outFile.delete();
    }
    System.setOut(new PrintStream(outFile))
    Console.withOut(System.out) _;
  }

  def outReset(): Unit = {
    System.out.flush();
    Console.flush();
    System.out.close();
    System.setOut(defaultOutput);
    Console.withOut(System.out) _;
  }
  
  def checkEPFD[EPFDTYPE <: ComponentDefinition : TypeTag](): Unit = {
    outToFile();
    validateAndPrint("Eventually Perfect Failure Detector", EPFDTester[EPFDTYPE]())
  }
  
  
  def checkBEB[BEBTYPE <: ComponentDefinition : TypeTag]() : Unit = {
    outToFile();
    validateAndPrint("Best Effort Broadcast", BEBTester[BEBTYPE]())
  }

  def checkRB[BEBTYPE <: ComponentDefinition : TypeTag, RBTYPE <: ComponentDefinition : TypeTag]() : Unit = {
    outToFile();
    validateAndPrint("Reliable Broadcast", RBTester[BEBTYPE, RBTYPE]())
  }

  def checkCRB[BEBTYPE <: ComponentDefinition : TypeTag, RBTYPE <: ComponentDefinition : TypeTag, CRBTYPE <: ComponentDefinition : TypeTag]() : Unit =  {
    outToFile();
    validateAndPrint("Causal Order Reliable Broadcast", CRBTester[BEBTYPE, RBTYPE, CRBTYPE]())
  }
  
  def checkNNAR[ARTYPE <: ComponentDefinition : TypeTag]() : Unit = {
    outToFile()
    validateAndPrint("NN Atomic Register", ABDTester[ARTYPE])
  };

  def checkSingleValueConsensus[ACTYPE <: ComponentDefinition: TypeTag]() : Unit = {
    outToFile();
    validateAndPrint("Single Value Consensus", PaxosTester[ACTYPE])
  } 
  
  def validateAndPrint(componentId: String, props: Iterable[ValidationGroup], maxPoints: Int = 1): Unit = {
    val passed = !props.exists(!_.isValid());
    outReset();
    println(outputLinkMsg)
    println(startframeMsg);
    props.foreach(println(_));
    println(finalMsg)
    if (passed) {
      println(successMsg(componentId));
      println(GradingToken(Encryptor.encrypt(gradeComponent(componentId, props, maxPoints).toJson.toString)).toJson.toString);
    }
    else {
      println(errorMsg(componentId));
    }
    println(endframeMsg);
  }
  
  
  def gradeComponent(id: String, results: Iterable[ValidationGroup], points : Int): ValidationReport = {
    ValidationReport(id, !results.exists(!_.isValid()),
      results.map(x => PropertyResult(x.id, x.isValid())), points);
  }
}







