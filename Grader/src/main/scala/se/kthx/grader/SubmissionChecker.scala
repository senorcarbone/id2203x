package se.kthx.grader

import akka.actor._
import akka.pattern.ask
import akka.pattern.pipe
import spray.http._
import spray.util._
import spray.http.HttpCharsets._
import spray.httpx.encoding._
import spray.httpx.marshalling._
import spray.httpx.unmarshalling._
import scala.concurrent._
import scala.concurrent.duration._
import akka.util.Timeout
import spray.json._
import spray.httpx.SprayJsonSupport._
import com.larskroll.common.ActorDispatcher
import se.kth.edx.id2203.validation.GradingToken
import se.kth.edx.id2203.validation.gradingFormat
import se.kth.edx.id2203.validation.Encryptor
import se.kth.edx.id2203.validation.ValidationUtils.ValidationReport
import se.kth.edx.id2203.validation.ValidationUtils.ValidationSimulation.reportFormat

object SubmissionChecker {
    case class CheckSubmission(entity: JsValue)
    case class FailedPull(msg: String)
    case class PartialGrade(title: String, msg: String, reports: Seq[ValidationReport])
    case class TokenPar(title: String, token: Array[Byte])
    def withManager(xfilemanager: ActorRef, grader: ActorRef): Props = Props(classOf[SubmissionChecker], xfilemanager, grader)
}

class SubmissionChecker(val xfilemanager: ActorRef, val grader: ActorRef) extends Actor with ActorLogging {
    import SubmissionChecker._;
    import XQueueJsonProtocol._;
    import XFileManager.Fetch;

    implicit val timeout = Timeout(60.seconds);
    implicit val ec = new ActorDispatcher(this)

    def receive = ActorDispatcher.receive orElse {
        case CheckSubmission(entity) => {
            try {
                val xqr = entity.convertTo[XQueueRequest];
                log.debug(s"Decoded request: $xqr")
                self ! xqr
            } catch {
                case error: Throwable =>
                    log.warning(s"Couldn't deserialise request:\n\t$entity \n Error was:\n\t$error");
                    sender ! XQueueManager.GradingCompleted // not really but this doesn't seem recoverable
            }
        }
        case xqr: XQueueRequest => {
            try {
                val components = xqr.xqueue_body.grader_payload.parseJson.convertTo[Array[String]].toSet;
                log.debug(s"To be graded: $components");
                val partialGradesIFFiles = xqr.xqueue_files map {
                    case (fileName, fileUri) =>
                        val f = (xfilemanager ? Fetch(fileUri)).mapTo[Either[HttpEntity, String]];
                        f map {
                            case Left(entity) => try {
                                val tokens = extractTokens(entity.asString);
                                PartialGrade(fileName, "none", tokens.map(decrypt))
                            } catch {
                                case e: Throwable =>
                                    log.debug(s"Couldn't deserialise notebook file: $entity");
                                    log.warning(s"Deserialisation Error was: $e");
                                    PartialGrade(fileName, "Data Format Invalid.", Seq.empty)
                            }
                            case Right(msg) => PartialGrade(fileName, "Unable to fetch result file.", Seq.empty)
                        } recover {
                            case e: Throwable =>
                                log.warning(s"An error occurred during processing of $fileName: $e");
                                PartialGrade(fileName, "An error occurred during processing.", Seq.empty)
                        }
                };
                val partialGradesIF = partialGradesIFFiles.toSeq :+ Future {
                    try {
                        val tokens = extractTokens(xqr.xqueue_body.student_response);
                        PartialGrade("student response", "none", tokens.map(decrypt))
                    } catch {
                        case e: Throwable =>
                            log.debug(s"Couldn't deserialise notebook file: ${xqr.xqueue_body.student_response}");
                            log.warning(s"Deserialisation Error was: $e");
                            PartialGrade("student response", "Data Format Invalid.", Seq.empty)
                    }
                };

                val partialGradesFI = Future.sequence(partialGradesIF);
                partialGradesFI map { partialGrades =>
                    val reports = partialGrades.foldLeft(Seq.empty[ValidationReport]) { (acc, pg) =>
                        acc ++ pg.reports
                    };
                    val result = if (!reports.isEmpty) {
                        grade(reports, components);
                    } else {
                        (0, "Messages: No grading tokens submitted. Please make sure that the tests pass locally before submitting the notebook.")
                    }
                    val messages = partialGrades.foldLeft("Other Messages:") { (acc, pg) =>
                        acc ++ "\n" ++ pg.title ++ ": " ++ pg.msg
                    }
                    val msg = nl2br(result._2 ++ "\n" ++ messages);
                    XQueueResponse(xqr.xqueue_header, XQueueResponseBody(result._1 > 0, result._1, msg))
                } recover {
                    case e =>
                        log.info(s"Error on grading: $e");
                        XQueueResponse(xqr.xqueue_header, XQueueResponseBody(false, 0, "There was an error grading the assigment. Please resubmit, if you believe this is an error on our side."))
                } pipeTo grader
            } catch {
                case e: Throwable =>
                    log.info(s"Error on grading component extraction: $e");
                    grader ! XQueueResponse(xqr.xqueue_header, XQueueResponseBody(false, 0, "There was an error grading the assigment. Error was: Nothing to grade"))
            }

        }
        case FailedPull(msg: String) => {
            log.warning(s"Got error on submission pull. Error was: $msg");
            sender ! XQueueManager.GradingCompleted // just to adjust the count
        }
    }

    private[grader] def extractTokens(data: String): Seq[TokenPar] = {
        val jso = data.parseJson.asJsObject
        jso.fields("paragraphs") match {
            case JsArray(els) => {
                val pars = els flatMap {
                    case paragraph: JsObject => {
                        val title = paragraph.fields.getOrElse("title", JsString("Untitled")).convertTo[String];
                        val result = paragraph.fields.getOrElse("result", JsObject.empty).asJsObject;
                        val msg = result.fields.getOrElse("msg", JsString("")).convertTo[String];
                        findToken(msg) match {
                            case Some(tokenS) => {
                                log.debug(s"Found grading token in message: $tokenS");
                                val gtoken = tokenS.parseJson.convertTo[GradingToken];
                                Some(TokenPar(title, gtoken.gradingToken))
                            }
                            case None => log.debug(s"Couldn't find grading token in $msg"); None
                        }
                    }
                    case x => log.debug(s"Invalid paragraph format in: $x"); None
                }
                pars.toSeq
            }
            case x => log.debug(s"No paragraphs found in: $x"); Seq.empty
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