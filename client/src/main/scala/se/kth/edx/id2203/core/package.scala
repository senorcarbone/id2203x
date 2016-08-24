package se.kth.edx.id2203

import scala.reflect.runtime.universe._
import se.sics.kompics.sl._
import se.sics.kompics.Kompics._;

package object core{

def runKompics[C <: ComponentDefinition: TypeTag] (){
  createAndStart(asJavaClass[C](typeOf[C]));
  waitForTermination();
}

}
