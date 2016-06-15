package se.kthx.id2203

import se.sics.kompics.sl._;
import se.sics.kompics.{ Kompics, KompicsEvent, Start };
import java.util.Random;

object Main {
    def main(args: Array[String]) {
        Kompics.createAndStart(classOf[ParentC]);
        Kompics.waitForTermination();
        println("Completed...shutting down");
        //System.exit(0);
    }
}

object State {
    type Initializer = ((Int, Int) => State);
}
trait State
case object Alive extends State {
    override def toString(): String = "x";
}
case object Dead extends State {
    override def toString(): String = " ";
}
case object Border extends State {
    override def toString(): String = "=";
}
case object Unknown extends State {
    override def toString(): String = "?";
}

case class BroadcastState(val generation: Long, val x: Int, val y: Int, val state: State) extends KompicsEvent
case class Progress(val generation: Long) extends KompicsEvent

object EnvironmentPort extends Port {
    indication[BroadcastState];
    indication[Progress];
}

class ParentC extends ComponentDefinition {

    val envOut = provides(EnvironmentPort);
    val envIn = requires(EnvironmentPort);

    val numGens = 100;
    val size = 20; // square size x size
    val rand = new Random(System.currentTimeMillis());
    val defaultInit: State.Initializer = (x: Int, y: Int) => {
        if (rand.nextBoolean()) {
            Alive
        } else {
            Dead
        }
    }

    val grid = Array.tabulate(size + 2, size + 2)((i, j) => {
        if (i == 0 || j == 0 || i == size + 1 || j == size + 1) {
            create(classOf[BorderC], new BorderInit(i - 1, j - 1));
        } else {
            create(classOf[CellC], new CellInit(i - 1, j - 1, defaultInit));
        }
    });
    for (x <- 0 until size) {
        for (y <- 0 until size) {
            val i = x + 1;
            val j = y + 1;
            val centre = grid(i)(j);
            connect(EnvironmentPort)(this.getComponentCore -> centre);
            connect(EnvironmentPort)(centre -> this.getComponentCore);
            //
            connect(EnvironmentPort)(grid(i - 1)(j - 1) -> centre);
            connect(EnvironmentPort)(grid(i - 1)(j) -> centre);
            connect(EnvironmentPort)(grid(i - 1)(j + 1) -> centre);
            connect(EnvironmentPort)(grid(i)(j - 1) -> centre);
            connect(EnvironmentPort)(grid(i)(j + 1) -> centre);
            connect(EnvironmentPort)(grid(i + 1)(j - 1) -> centre);
            connect(EnvironmentPort)(grid(i + 1)(j) -> centre);
            connect(EnvironmentPort)(grid(i + 1)(j + 1) -> centre);
        }
    }
    val stateGrid = Array.fill[State](size, size) { Unknown }
    val gridSize = size * size;
    var broadcastCount = 0l;

    private var generation = -1l

    ctrl uponEvent {
        case _: Start => handle {
            println("Starting generation 0...");
            generation = 0;
            trigger(Progress(generation) -> envOut);
        }
    }
    envIn uponEvent {
        case BroadcastState(gen, x, y, state) => handle {
            if (x >= 0 && y >= 0 && x < size && y < size) {
                stateGrid(x)(y) = state;
                broadcastCount += 1;
            }
            if (broadcastCount >= gridSize) {
                println(s"Generation $generation:\n${stateGrid.deep.mkString("\n")}");
                broadcastCount = 0l;
                if (generation < numGens) {
                    generation += 1;
                    trigger(Progress(generation) -> envOut);
                } else {
                    Kompics.asyncShutdown();
                }
            }
        }
    }
}

class CellInit(val x: Int, val y: Int, val initialState: State.Initializer) extends se.sics.kompics.Init[CellC] {
    def getInitialState(): State = initialState(x, y);
}

class CellC(init: CellInit) extends ComponentDefinition {
    val xPos = init.x;
    val yPos = init.y;
    private var state: State = init.getInitialState();
    private val view = scala.collection.mutable.Map.empty[Tuple2[Int, Int], State];
    private var lastGen = -1l;

    val envIn = requires(EnvironmentPort);
    val envOut = provides(EnvironmentPort);
    envIn uponEvent {
        case Progress(generation) => handle {
            //println(s"Cell($xPos, $yPos) starting generation $generation as $state");
            trigger(BroadcastState(generation, xPos, yPos, state) -> envOut);
            if (lastGen < generation) {
                view.clear();
                lastGen = generation;
            }
        }
        case BroadcastState(generation, x, y, otherState) => handle {
            if (lastGen < generation) {
                view.clear();
                lastGen = generation;
            }
            view += ((x -> y) -> otherState);
            //println(s"Cell($xPos, $yPos) got state $otherState from Cell($x, $y) leading to: $view");
            if (view.size == 8) { // everyone around me 3x3-1
                var liveC = 0;
                for (s <- view.values) {
                    s match {
                        case Alive         => liveC += 1;
                        case Dead | Border => // ignore
                    }
                }
                state match {
                    case Alive => {
                        if (liveC < 2) { // Rule 1
                            state = Dead;
                        } else if (liveC > 3) { // Rule 3
                            state = Dead;
                        }
                        // Rule 2
                    }
                    case Dead => {
                        if (liveC == 3) {
                            state = Alive; // Rule 4
                        }
                    }
                    case _ => new RuntimeException("Cells are either Dead or Alive...not Borders!")
                }
            }
        }
    }
}

class BorderInit(val x: Int, val y: Int) extends se.sics.kompics.Init[BorderC] {

}

class BorderC(init: BorderInit) extends ComponentDefinition {
    val xPos = init.x;
    val yPos = init.y;

    val envIn = requires(EnvironmentPort);
    val envOut = provides(EnvironmentPort);

    envIn uponEvent {
        case Progress(generation) => handle {
            trigger(BroadcastState(generation, xPos, yPos, Border) -> envOut);
        }
    }
}