package se.kth.edx.id2203.validation

import scala.reflect.runtime.universe._
import se.kth.edx.id2203.validation.testers.EPFDTester
import se.sics.kompics.sl._
import se.sics.kompics.{ComponentDefinition => _, Port => _, _}

import scala.reflect.ClassTag

object validation {
  
  def checkEPFD[T <: ComponentDefinition : TypeTag]() : Unit = {
       EPFDTester[T]();
  }
  
}







