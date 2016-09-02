package se.kth.edx.id2203

import scala.reflect.runtime.universe._
import se.sics.kompics.sl._;
import se.sics.kompics.{ Kompics, FaultHandler };
import se.sics.kompics.Fault;
import se.sics.kompics.Fault.ResolveAction;

package object core {

    private val nonCrashingFaultHandler = new FaultHandler() {
        override def handle(f: Fault): ResolveAction = {
            println(s"Execution aborted due to uncaught fault:\n$f");
            ResolveAction.DESTROY
        }
    };

    def runKompics[C <: ComponentDefinition: TypeTag]() {
        Kompics.setFaultHandler(nonCrashingFaultHandler);
        Kompics.createAndStart(asJavaClass[C](typeOf[C]));
        Kompics.waitForTermination();
    }

    def runKompics[C <: ComponentDefinition: TypeTag](init: se.sics.kompics.Init[C]) {
        Kompics.setFaultHandler(nonCrashingFaultHandler);
        Kompics.createAndStart(asJavaClass[C](typeOf[C]), init);
        Kompics.waitForTermination();
    }

}
