package se.kthx.grader

import com.typesafe.scalalogging.StrictLogging
import java.io.{ File, FileWriter, PrintWriter, StringWriter }
import java.util.concurrent.Executors
import scala.concurrent.{ ExecutionContext, Future, Await }
import scala.concurrent.duration.{ span => _, _ }
import scala.util.{ Try, Success, Failure }
import scalatags.Text.all._

class Grader(val srcDir: File) extends StrictLogging {

  //val threadpool = Executors.newFixedThreadPool(8);
  val threadpool = Executors.newSingleThreadExecutor();
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(threadpool);

  def kill(): Unit = {
    threadpool.shutdown()
  }

  def grade(): Try[List[(String, Try[SubmissionChecker.PartialGrade])]] = {
    val jsonFiles = srcDir.listFiles().flatMap { f =>
      if (f.isFile()) {
        if (f.getName.endsWith(".json")) {
          Some(f)
        } else {
          logger.warn(s"Ignoring ${f.getName} as it's not a JSON file.");
          None
        }
      } else {
        logger.warn(s"Ignoring ${f.getName} as it's not a file.");
        None
      }
    };
    val resAF = jsonFiles.map { f =>
      Future {
        val sc = SubmissionChecker();
        (f.getName, sc.check(f))
      }
    }.toList;
    val resF = Future.sequence(resAF);
    Await.ready(resF, 1.minute);
    resF.value.get
  }

  def asHTML(): Try[File] = {
    grade.flatMap(res => {
      val f = File.createTempFile("grades", ".html");
      val cont = table(
        tr(th("File"), th("Result"), th("Message")),
        res.map {
          case (name, Success(grade)) => {
            val report = grade.reports.foldLeft((0, "Messages:")) { (acc, report) =>
              (acc._1 + report.points, acc._2 ++ "\n" ++ report.toMessage)
            };
            tr(td(name), td(span(s"Points: ${report._1}")), td(p(grade.msg), p(report._2)))
          }
          case (name, Failure(e)) => {
            val sw = new StringWriter();
            e.printStackTrace(new PrintWriter(sw));
            tr(td(name), td(e.toString), td(sw.toString))
          }
        });
      val html = HTMLLayout.base(HTMLLayout.defaultHeader, cont).render;
      // This possibly creates a FileWriter, but maybe not
      val writer = Try(new FileWriter(f))
      // If it did, we use it to write the data and return it again
      // If it didn't we skip the map and print the exception and return the original, just in-case it was actually .write() that failed
      // Then we close the file
      writer.map(w => { w.write(html); w }).recoverWith{ case e => { e.printStackTrace(); writer } }.map(_.close).map(_ => f)
    })
  }
}

object Grader {
  def withSources(srcDir: File): Grader = new Grader(srcDir);
}

