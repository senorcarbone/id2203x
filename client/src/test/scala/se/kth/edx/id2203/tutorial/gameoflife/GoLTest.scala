package se.kth.edx.id2203.tutorial.gameoflife

import org.scalatest._
import java.util.Random;
import se.sics.kompics.sl._
import se.sics.kompics.{ Kompics }

case class CellCInit(x: Int, y: Int, init: State.Initializer) extends se.sics.kompics.Init[Cell]

class CellC(init: CellCInit) extends Cell {
    val xPos = init.x;
    val yPos = init.y;
    private var state: State = init.init.apply(xPos, yPos);
    private val view = scala.collection.mutable.Map.empty[Tuple2[Int, Int], State];
    private var lastGen = -1l;

    val envIn = requires(EnvironmentPort);
    val envOut = provides(EnvironmentPort);
    envIn uponEvent {
        case Progress(generation) => {
            //println(s"Cell($xPos, $yPos) starting generation $generation as $state");
            trigger(BroadcastState(generation, xPos, yPos, state) -> envOut);
            if (lastGen < generation) {
                view.clear();
                lastGen = generation;
            }
        }
        case BroadcastState(generation, x, y, otherState) => {
            if (lastGen < generation) {
                view.clear();
                lastGen = generation;
            }
            view += ((x -> y) -> otherState);
            //println(s"Cell($xPos, $yPos) got state $otherState from Cell($x, $y) leading to: $view");
            if (view.size == 8) { // everyone around me 3x3-1
                //                var liveC = 0;
                //                for (s <- view.values) {
                //                    s match {
                //                        case Alive => liveC += 1;
                //                        case _ => // ignore
                //                    }
                //                }
                val liveC = view.values.count {
                    case Alive => true
                    case _     => false
                };
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
                    case _ => new RuntimeException("Cells are either Dead or Alive...nothing else!")
                }
            }
        }
    }
}

class GoLTest extends FlatSpec with Matchers {
    val rand = new Random(System.currentTimeMillis());
    val defaultInit: State.Initializer = (x: Int, y: Int) => {
        if (rand.nextBoolean()) {
            Alive
        } else {
            Dead
        }
    }
    val toCellCInit: Cell.Initializer = {
        case (x: Int, y: Int, init: State.Initializer) => CellCInit(x, y, init).asInstanceOf[se.sics.kompics.Init[Cell]]
    };

    "Game of Life" should "run" in {
        val cellclass = classOf[CellC].asInstanceOf[Class[Cell]]
        Kompics.createAndStart(classOf[ParentC], GameOfLifeInit(defaultInit, cellclass, toCellCInit, numGenerations = 10), 8);
        Kompics.waitForTermination();
    }
}
