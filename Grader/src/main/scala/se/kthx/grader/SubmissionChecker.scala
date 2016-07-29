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

object SubmissionChecker {
    case class CheckSubmission(entity: HttpEntity)
    case class FailedPull(msg: String)
    case class PartialGrade(title: String, msg: String, points: Int)
    def withManager(xfilemanager: ActorRef, grader: ActorRef): Props = Props(classOf[SubmissionChecker], xfilemanager, grader)
}

class SubmissionChecker(val xfilemanager: ActorRef, val grader: ActorRef) extends Actor with ActorLogging {
    import SubmissionChecker._;
    import XQueueJsonProtocol._;
    import XFileManager.Fetch;
    
    implicit val timeout = Timeout(60.seconds);
    implicit val ec = context.dispatcher;
    

    def receive = {
        case CheckSubmission(entity) => {
            entity.as[XQueueRequest] match {
                case Right(xqr) => self ! xqr
                case Left(error) =>
                    log.warning(s"Couldn't deserialise request:\n\t$entity \n Error was:\n\t$error");
            }
        }
        case xqr: XQueueRequest => {
            val partialGradesIF = xqr.xqueue_files map {
                case (fileName, fileUri) =>
                    val f = (xfilemanager ? Fetch(fileUri)).mapTo[Either[HttpEntity, String]];
                    f map {
                        case Left(entity) => try {
                            val jso = entity.asString.parseJson.asJsObject
                            jso.fields("paragraphs") match {
                                case JsArray(els) => {
                                    val pars = els map {
                                        case paragraph: JsObject => {
                                            val title = paragraph.fields.getOrElse("title", JsString("Untitled")).convertTo[String];
                                            val result = paragraph.fields("result").asJsObject;
                                            val msg = result.fields("msg").convertTo[String];
                                            val realMsg = decrypt(msg);
                                            PartialGrade(title, "ok", grade(realMsg))
                                        }
                                        case _ => PartialGrade("?", "Invalid format!", 0)
                                    }
                                    pars.foldLeft(PartialGrade(fileName, "Messages:", 0)) { (acc, pg) =>
                                        PartialGrade(acc.title, acc.msg ++ "\n" ++ pg.msg, acc.points + pg.points)
                                    }
                                }
                                case _ => PartialGrade(fileName, "No Content", 0)
                            }
                        } catch {
                            case e: Throwable =>
                                log.debug(s"Couldn't deserialise notebook file: $entity");
                                log.warning(s"Deserialisation Error was: $e");
                                PartialGrade(fileName, "Data Format Invalid.", 0)
                        }
                        case Right(msg) => PartialGrade(fileName, "Unable to fetch result file.", 0)
                    }
            };
            val partialGradesFI = Future.sequence(partialGradesIF);
            partialGradesFI map { partialGrades =>
                val totalGrade = partialGrades.foldLeft((0, "Messages:")) { (acc, pg) =>
                    (acc._1 + pg.points, acc._2 ++ "/n" ++ pg.title ++ ": " ++ pg.msg)
                }
                //TODO check success condition
                XQueueResponse(true, totalGrade._1, totalGrade._2)
            } recover {
                case e =>
                    log.info(s"Error on grading: $e");
                    XQueueResponse(false, 0, "There was an error grading the assigment. Please resubmit, if you believe this is an error on our side.")
            } pipeTo grader

        }
        case FailedPull(msg: String) => {
            log.warning(s"Got error on submission pull. Error was: $msg");
        }
    }

    private def decrypt(data: String): String = {
        return data;
    }

    private def grade(results: String): Int = {
        return 1;
    }
}