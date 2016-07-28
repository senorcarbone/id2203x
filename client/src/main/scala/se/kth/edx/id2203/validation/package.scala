package se.kth.edx.id2203.validation

import scala.reflect.runtime.universe._
import se.kth.edx.id2203.validation.testers.EPFDTester
import se.sics.kompics.sl._
import se.sics.kompics.{ComponentDefinition => _, Port => _, _}


object validation {
  
  def checkEPFD[T <: ComponentDefinition : TypeTag]() : Unit = {
      val props =  EPFDTester[T]();
      val passed = !props.exists(!_.isValid());
      println(props.toString());
//      if(passed){
        println("Congratsulations! You have passed this part. A unique token has been generated for your submission. Please do not edit.");
        //TODO add random number
        println(Encryptor.encrypt(props.toString()));
//      }
    
  }
  
}







