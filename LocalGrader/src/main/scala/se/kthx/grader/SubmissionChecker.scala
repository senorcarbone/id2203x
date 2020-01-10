package se.kthx.grader

import java.io.File
import com.typesafe.scalalogging.StrictLogging
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{ Try, Success, Failure }
import scala.io.Source
import spray.json._
import com.larskroll.common.ActorDispatcher
import se.kth.edx.id2203.validation.GradingToken
import se.kth.edx.id2203.validation.gradingFormat
import se.kth.edx.id2203.validation.Encryptor
import se.kth.edx.id2203.validation.ValidationUtils.ValidationReport
import se.kth.edx.id2203.validation.ValidationUtils.ValidationSimulation.reportFormat
import org.slf4j.MDC

object SubmissionChecker {
  case class CheckSubmission(entity: JsValue)
  case class FailedPull(msg: String)
  case class PartialGrade(title: String, msg: String, reports: Seq[ValidationReport])
  case class TokenPar(title: String, token: Array[Byte])

  def apply(): SubmissionChecker = new SubmissionChecker();
}

class SubmissionChecker extends StrictLogging {
  import SubmissionChecker._;
  import DefaultJsonProtocol._

  def check(notebook: File): Try[PartialGrade] = {
    MDC.put("file", notebook.getName);

    for {
      data <- read(notebook);
      grades <- check(data);
      _ <- Try(MDC.clear())
    } yield PartialGrade(notebook.getName, "none", grades)
  }

  def read(f: File): Try[String] = {
    Try {
      logger.debug(s"Reading file ${f.getName}");
      Source.fromFile(f).mkString
    }
  }

  def check(s: String): Try[Seq[ValidationReport]] = {
    //logger.debug("Checking Notebook:\n${s}");
    Try {
      val tokens = extractTokens(s);
      tokens.map(decrypt)
    }
  }

  //  def receive = ActorDispatcher.receive orElse {
  //    case CheckSubmission(entity) => {
  //      try {
  //        val xqr = entity.convertTo[XQueueRequest];
  //        log.debug(s"Decoded request: $xqr")
  //        self ! xqr
  //      } catch {
  //        case error: Throwable =>
  //          log.warning(s"Couldn't deserialise request:\n\t$entity \n Error was:\n\t$error");
  //          sender ! XQueueManager.GradingCompleted // not really but this doesn't seem recoverable
  //      }
  //    }
  //    case xqr: XQueueRequest => {
  //      try {
  //        val components = xqr.xqueue_body.grader_payload.parseJson.convertTo[Array[String]].toSet;
  //        log.debug(s"To be graded: $components");
  //        val partialGradesIFFiles = xqr.xqueue_files map {
  //          case (fileName, fileUri) =>
  //            val f = (xfilemanager ? Fetch(fileUri)).mapTo[Either[HttpEntity, String]];
  //            f map {
  //              case Left(entity) => try {
  //                val tokens = extractTokens(entity.asString);
  //                PartialGrade(fileName, "none", tokens.map(decrypt))
  //              } catch {
  //                case e: Throwable =>
  //                  log.debug(s"Couldn't deserialise notebook file: $entity");
  //                  log.warning(s"Deserialisation Error was: $e");
  //                  PartialGrade(fileName, "Data Format Invalid.", Seq.empty)
  //              }
  //              case Right(msg) => PartialGrade(fileName, "Unable to fetch result file.", Seq.empty)
  //            } recover {
  //              case e: Throwable =>
  //                log.warning(s"An error occurred during processing of $fileName: $e");
  //                PartialGrade(fileName, "An error occurred during processing.", Seq.empty)
  //            }
  //        };
  //        val partialGradesIF = partialGradesIFFiles.toSeq :+ Future {
  //          try {
  //            val tokens = extractTokens(xqr.xqueue_body.student_response);
  //            PartialGrade("student response", "none", tokens.map(decrypt))
  //          } catch {
  //            case e: Throwable =>
  //              log.debug(s"Couldn't deserialise notebook file: ${xqr.xqueue_body.student_response}");
  //              log.warning(s"Deserialisation Error was: $e");
  //              PartialGrade("student response", "Data Format Invalid.", Seq.empty)
  //          }
  //        };
  //
  //        val partialGradesFI = Future.sequence(partialGradesIF);
  //        partialGradesFI map { partialGrades =>
  //          val reports = partialGrades.foldLeft(Seq.empty[ValidationReport]) { (acc, pg) =>
  //            acc ++ pg.reports
  //          };
  //          val result = if (!reports.isEmpty) {
  //            grade(reports, components);
  //          } else {
  //            (0, "Messages: No grading tokens submitted. Please make sure that the tests pass locally before submitting the notebook.")
  //          }
  //          val messages = partialGrades.foldLeft("Other Messages:") { (acc, pg) =>
  //            acc ++ "\n" ++ pg.title ++ ": " ++ pg.msg
  //          }
  //          val msg = nl2br(result._2 ++ "\n" ++ messages);
  //          XQueueResponse(xqr.xqueue_header, XQueueResponseBody(result._1 > 0, result._1, msg))
  //        } recover {
  //          case e =>
  //            log.info(s"Error on grading: $e");
  //            XQueueResponse(xqr.xqueue_header, XQueueResponseBody(false, 0, "There was an error grading the assigment. Please resubmit, if you believe this is an error on our side."))
  //        } pipeTo grader
  //      } catch {
  //        case e: Throwable =>
  //          log.info(s"Error on grading component extraction: $e");
  //          grader ! XQueueResponse(xqr.xqueue_header, XQueueResponseBody(false, 0, "There was an error grading the assigment. Error was: Nothing to grade"))
  //      }
  //
  //    }
  //    case FailedPull(msg: String) => {
  //      log.warning(s"Got error on submission pull. Error was: $msg");
  //      sender ! XQueueManager.GradingCompleted // just to adjust the count
  //    }
  //  }

  private[grader] def extractTokens(data: String): Seq[TokenPar] = {
    val firstBrace = data.indexOf('{');
    val lastBrace = data.lastIndexOf('}');
    //    if ((firstBrace < 0) || (lastBrace < 0)) {
    //      log.warning(s"JSON content was not found {=$firstBrace and }=$lastBrace");
    //      return Seq.empty;
    //    }
    val jsonString = data.substring(firstBrace, lastBrace + 1);
    val jso = jsonString.parseJson.asJsObject;
    jso.fields("paragraphs") match {
      case JsArray(els) => {
        val pars = els flatMap {
          case paragraph: JsObject => {
            val title = paragraph.fields.getOrElse("title", JsString("Untitled")).convertTo[String];
            val resultsF = paragraph.fields.getOrElse("results", JsObject.empty); //.asJsObject;
            val results = try {
              resultsF.asJsObject
            } catch {
              case _: DeserializationException => {
                logger.warn("Invalid result object: {}", resultsF);
                JsObject.empty
              }
              case e: Throwable => {
                logger.warn("Unexpected exception for result: {}", resultsF);
                throw e;
              }
            };
            results.fields.get("msg") match {
              case Some(JsArray(resultList)) => resultList flatMap {
                case result: JsObject => {
                  result.fields("type") match {
                    case JsString("HTML") => {
                      val msg = result.fields.getOrElse("data", JsString("")).convertTo[String];
                      findToken(msg) match {
                        case Some(tokenS) => {
                          logger.debug(s"Found grading token in message: $tokenS");
                          val gtoken = tokenS.parseJson.convertTo[GradingToken];
                          Some(TokenPar(title, gtoken.gradingToken))
                        }
                        case None => logger.debug(s"Couldn't find grading token in $msg"); None
                      }
                    }
                    case x => logger.debug(s"Invalid result type in: $x"); None
                  }
                }
                case x => logger.debug(s"Invalid result format in: $x"); None
              }
              case x => logger.debug(s"Invalid result format in: $x"); Seq.empty
            }
          }
          case x => logger.debug(s"Invalid paragraph format in: $x"); None
        }
        pars.toSeq
      }
      case x => logger.debug(s"No paragraphs found in: $x"); Seq.empty
    }
  }

  private[grader] def findToken(resultMsg: String): Option[String] = {
    val start = resultMsg.indexOf("{\"gradingToken\"");
    if (start >= 0) {
      val tail = resultMsg.substring(start);
      val end = tail.indexOf("]}");
      if (end >= 0) {
        Some(tail.substring(0, end + 2))
      } else {
        None
      }
    } else {
      None
    }
  }

  private[grader] def decrypt(token: TokenPar): ValidationReport = {
    val reportS = Encryptor.decryptText(token.token);
    val report = reportS.parseJson.convertTo[ValidationReport];
    report
  }

  private[grader] def grade(reports: Seq[ValidationReport], components: Set[String]): Tuple2[Int, String] = {
    val rMap = reports.map { r => (r.id -> r) }.toMap; // remove duplicate entries
    val fMap = rMap.filterKeys { id => components.contains(id) } // remove entries from other tasks
    if (fMap.isEmpty) {
      (0, "Messages: It looks like you submitted the wrong task.")
    } else {
      fMap.values.foldLeft((0, "Messages:")) { (acc, report) =>
        (acc._1 + report.points, acc._2 ++ "\n" ++ report.toMessage)
      }
    }
  }
}
