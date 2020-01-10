package se.kthx.grader

import org.rogach.scallop._
import java.io.File
import java.awt.Desktop
import org.slf4j.LoggerFactory
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.core.util.StatusPrinter;
import scala.util.{ Try, Success, Failure }

class CmdOpts(arguments: Seq[String]) extends ScallopConf(arguments) {
  val srcDir = opt[File](required = true, descr = "Directory where the json workbooks to be graded are stored.");
  validateFileExists(srcDir);
  validateFileIsDirectory(srcDir);
  verify()
}

object Main {
  def main(args: Array[String]) {
    // assume SLF4J is bound to logback in the current environment
    val lc = LoggerFactory.getILoggerFactory().asInstanceOf[LoggerContext];
    // print logback's internal status
    StatusPrinter.print(lc);

    val opts = new CmdOpts(args);
    val grader = Grader.withSources(opts.srcDir());
    val resT = grader.asHTML();
    resT match {
      case Success(res) => {
        println("### Ok, got some results:");
        Desktop.getDesktop.open(res);
        //            res.foreach {
        //              case (file, Success(grade)) => println(s"$file -> $grade")
        //              case (file, Failure(e)) => {
        //                println(s"$file -> ${e}");
        //                e.printStackTrace(Console.err);
        //              }
        //            }
      }
      case Failure(e) => {
        Console.err.println("Something went terribly wrong!");
        e.printStackTrace(Console.err);
      }
    }
    grader.kill();
  }
}
