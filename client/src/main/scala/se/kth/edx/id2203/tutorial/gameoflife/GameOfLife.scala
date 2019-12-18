package se.kth.edx.id2203.tutorial.gameoflife;

import se.sics.kompics.sl._;
import se.sics.kompics.{ Kompics, KompicsEvent, Start };
import spray.json._

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
case object Unknown extends State {
    override def toString(): String = "?";
}
case class BroadcastState(val generation: Long, val x: Int, val y: Int, val state: State) extends KompicsEvent
case class Progress(val generation: Long) extends KompicsEvent

object EnvironmentPort extends Port {
    indication[BroadcastState];
    indication[Progress];
}

object StateJsonProtocol extends DefaultJsonProtocol {
    implicit object StateFormat extends JsonFormat[State] {
        override def write(state: State) = {
            state match {
                case Alive => JsNumber(1)
                case Dead  => JsNumber(0)
                case _     => JsNumber(-1)
            }
        }

        override def read(v: JsValue): State = {
            v match {
                case JsNumber(i) if i.intValue == 1 => Alive
                case JsNumber(i) if i.intValue == 0 => Dead
                case _           => Unknown
            }
        }
    }
}

case class GameOfLifeInit(
    initializer: State.Initializer,
    cellType: Class[Cell], cellInit: Cell.Initializer,
    numGenerations: Int = 100,
    size: Int = 20) extends se.sics.kompics.Init[ParentC]

object Cell {
    type Initializer = Tuple3[Int, Int, State.Initializer] => se.sics.kompics.Init[Cell]
}

abstract class Cell extends ComponentDefinition {

}

class ParentC(init: GameOfLifeInit) extends ComponentDefinition {
    import StateJsonProtocol._
    
    val envOut = provides(EnvironmentPort);
    val envIn = requires(EnvironmentPort);

    val size = init.size;

    val grid = Array.tabulate(size, size)((i, j) => {
        create(init.cellType, init.cellInit((i, j, init.initializer)));
    });
    val wrap = stateWrap(size);
    for (i <- 0 until size) {
        for (j <- 0 until size) {
            val centre = grid(i)(j);
            connect(EnvironmentPort)(this.getComponentCore -> centre);
            connect(EnvironmentPort)(centre -> this.getComponentCore);
            // make sure we can't connect to ourselves
            assert(wrap(i - 1) != i);
            assert(wrap(i + 1) != i);
            assert(wrap(j - 1) != j);
            assert(wrap(j + 1) != j);
            // connect
            connect(EnvironmentPort)(grid(wrap(i - 1))(wrap(j - 1)) -> centre);
            connect(EnvironmentPort)(grid(wrap(i - 1))(j) -> centre);
            connect(EnvironmentPort)(grid(wrap(i - 1))(wrap(j + 1)) -> centre);
            connect(EnvironmentPort)(grid(i)(wrap(j - 1)) -> centre);
            connect(EnvironmentPort)(grid(i)(wrap(j + 1)) -> centre);
            connect(EnvironmentPort)(grid(wrap(i + 1))(wrap(j - 1)) -> centre);
            connect(EnvironmentPort)(grid(wrap(i + 1))(j) -> centre);
            connect(EnvironmentPort)(grid(wrap(i + 1))(wrap(j + 1)) -> centre);
        }
    }
    val stateGrid = Array.fill[State](size, size) { Unknown }

    val stateHistory = collection.mutable.ArrayBuffer.empty[Array[Array[State]]];
    val gridSize = size * size;
    var broadcastCount = 0l;

    private var generation = -1l

    ctrl uponEvent {
        case _: Start => {
            println("Starting generation 0...");
            generation = 0;
            trigger(Progress(generation) -> envOut);
        }
    }
    envIn uponEvent {
        case BroadcastState(gen, x, y, state) => {
            if (x >= 0 && y >= 0 && x < size && y < size) {
                stateGrid(x)(y) = state;
                broadcastCount += 1;
            }
            if (broadcastCount >= gridSize) {
                val sgc = stateGrid.map(_.clone);
                stateHistory += sgc;
                println(s"Completed generation $generation");
                broadcastCount = 0l;
                if (generation < init.numGenerations) {
                    generation += 1;
                    println(s"Starting generation $generation");
                    trigger(Progress(generation) -> envOut);
                } else {
                    println("Finishing up.");
                    val hist = stateHistory.toList;
//                    println("****** ORIGINAL HISTORY ********")
//                    hist.foreach { x => println(s"${x.deep.mkString("\n")}"); }
                    val js = hist.toJson.compactPrint;
//                    val hist2 = js.parseJson.convertTo[List[Array[Array[State]]]];
//                    println("****** RECONSTRUCTED HISTORY ********")
//                    hist2.foreach { x => println(s"${x.deep.mkString("\n")}"); }
                    HTMLRenderer.render(js);
                    Kompics.asyncShutdown();
                }
            }
        }
    }
}
//
//class CellInit(val x: Int, val y: Int, val initialState: State.Initializer) extends se.sics.kompics.Init[CellC] {
//  def getInitialState(): State = initialState(x, y);
//}
//
//class CellC(init: CellInit) extends ComponentDefinition {
//  val xPos = init.x;
//  val yPos = init.y;
//  private var state: State = init.getInitialState();
//  private val view = scala.collection.mutable.Map.empty[Tuple2[Int, Int], State];
//  private var lastGen = -1l;
//
//  val envIn = requires(EnvironmentPort);
//  val envOut = provides(EnvironmentPort);
//  envIn uponEvent {
//    case Progress(generation) => {
//      //println(s"Cell($xPos, $yPos) starting generation $generation as $state");
//      trigger(BroadcastState(generation, xPos, yPos, state) -> envOut);
//      if (lastGen < generation) {
//        view.clear();
//        lastGen = generation;
//      }
//    }
//    case BroadcastState(generation, x, y, otherState) => {
//      if (lastGen < generation) {
//        view.clear();
//        lastGen = generation;
//      }
//      view += ((x -> y) -> otherState);
//      //println(s"Cell($xPos, $yPos) got state $otherState from Cell($x, $y) leading to: $view");
//      if (view.size == 8) { // everyone around me 3x3-1
//      var liveC = 0;
//        for (s <- view.values) {
//          s match {
//            case Alive         => liveC += 1;
//            case Dead | Border => // ignore
//          }
//        }
//        state match {
//          case Alive => {
//            if (liveC < 2) { // Rule 1
//              state = Dead;
//            } else if (liveC > 3) { // Rule 3
//              state = Dead;
//            }
//            // Rule 2
//          }
//          case Dead => {
//            if (liveC == 3) {
//              state = Alive; // Rule 4
//            }
//          }
//          case _ => new RuntimeException("Cells are either Dead or Alive...not Borders!")
//        }
//      }
//    }
//  }
//}
//
//class BorderInit(val x: Int, val y: Int) extends se.sics.kompics.Init[BorderC] {
//
//}
//
//class BorderC(init: BorderInit) extends ComponentDefinition {
//  val xPos = init.x;
//  val yPos = init.y;
//
//  val envIn = requires(EnvironmentPort);
//  val envOut = provides(EnvironmentPort);
//
//  envIn uponEvent {
//    case Progress(generation) => {
//      trigger(BroadcastState(generation, xPos, yPos, Border) -> envOut);
//    }
//  }
//}
//
//object GameOfLife extends App{
//  Kompics.shutdown();
//  Kompics.createAndStart(classOf[ParentC]);
//  Kompics.waitForTermination();
//  println("Completed...shutting down");
//}
//
