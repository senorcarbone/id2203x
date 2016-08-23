package se.kth.edx.id2203.core

import se.sics.kompics.sl.ComponentDefinition
import se.sics.kompics.Kompics._;

object Kompics {
  
  def run[C <: ComponentDefinition](componentClass: Class[C]) : Unit = 
  {
    createAndStart(componentClass);
    waitForTermination();
  }

}
