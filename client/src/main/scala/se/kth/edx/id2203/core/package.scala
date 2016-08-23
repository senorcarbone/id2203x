package se.kth.edx.id2203

import scala.reflect.runtime.universe._
import se.sics.kompics.sl._
import se.sics.kompics.Kompics;

package object core{

def runKompics[C <: ComponentDefinition: TypeTag] (){
  Kompics.createAndStart(asJavaClass[C](typeOf[C]));
  Kompics.waitForTermination();
}

}
