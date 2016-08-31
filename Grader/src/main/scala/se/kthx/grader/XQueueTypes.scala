package se.kthx.grader

import spray.json._

case class XQueueStringResponse(return_code: Int, content: String)
case class XQueueIntResponse(return_code: Int, content: Int)
case class XQueueResponse(xqueue_header: XQueueHeader, xqueue_body: XQueueResponseBody)
case class XQueueResponseBody(correct: Boolean, score: Int, msg: String, grader_id: String = "KTHx Grader");
case class XQueueRequest(xqueue_header: XQueueHeader, xqueue_files: Map[String, String], xqueue_body: XQueueBody)
case class XQueueHeader(submission_id: Long, submission_key: String)
case class XQueueBody(student_info: StudentInfo, student_response: String, grader_payload: String)
case class StudentInfo(anonymous_student_id: String, submission_time: String, random_seed: Long)

object XQueueJsonProtocol extends DefaultJsonProtocol {
    implicit val stringRFormat = jsonFormat2(XQueueStringResponse);
    implicit val intRFormat = jsonFormat2(XQueueIntResponse);
    implicit val responseBodyFormat = jsonFormat4(XQueueResponseBody);
    //    implicit val infoFormat = jsonFormat3(StudentInfo);
    //    implicit val headerFormat = jsonFormat2(XQueueHeader);
    //    implicit val bodyFormat = jsonFormat3(XQueueBody);
    //    implicit val requestFormat = jsonFormat3(XQueueRequest);
    implicit object StudentInfoJsonFormat extends RootJsonFormat[StudentInfo] {
        def write(si: StudentInfo) = ??? // don't need to write this

        def read(value: JsValue) = value match {
            case jso: JsObject =>
                val sio = for (
                    JsString(id) <- jso.fields.get("anonymous_student_id");
                    JsString(time) <- jso.fields.get("submission_time");
                    JsNumber(seed) <- jso.fields.get("random_seed")
                ) yield StudentInfo(id, time, seed.toLong);
                sio match {
                    case Some(si) => si
                    case _        => deserializationError(s"Invalid StudentInfo object: $value")
                }
            case _ => deserializationError(s"StudentInfo object expected, but got $value")
        }
    }
    implicit object XQBodyJsonFormat extends RootJsonFormat[XQueueBody] {
        def write(si: XQueueBody) = ??? // don't need to write this

        def read(value: JsValue) = value match {
            case jso: JsObject =>
                val xqbo = for (
                    JsString(infoS) <- jso.fields.get("student_info");
                    JsString(response) <- jso.fields.get("student_response");
                    JsString(payload) <- jso.fields.get("grader_payload")
                ) yield XQueueBody(infoS.parseJson.convertTo[StudentInfo], response, payload);
                xqbo match {
                    case Some(xqb) => xqb
                    case _         => deserializationError(s"Invalid XQueueBody object: $value")
                }
            case _ => deserializationError(s"XQueueBody object expected, but got $value")
        }
    }
    implicit object XQHeaderJsonFormat extends RootJsonFormat[XQueueHeader] {
        def write(si: XQueueHeader) = {
            val content = Map(
                ("submission_id" -> JsNumber(si.submission_id)),
                ("submission_key") -> JsString(si.submission_key));
            JsObject(content)
        }

        def read(value: JsValue) = value match {
            case jso: JsObject =>
                val xqho = for (
                    JsNumber(id) <- jso.fields.get("submission_id");
                    JsString(key) <- jso.fields.get("submission_key")
                ) yield XQueueHeader(id.toLongExact, key);
                xqho match {
                    case Some(xqh) => xqh
                    case _         => deserializationError(s"Invalid XQueueHeader object: $value")
                }
            case _ => deserializationError(s"XQueueHeader object expected, but got $value")
        }
    }
    implicit object XQRequestJsonFormat extends RootJsonFormat[XQueueRequest] {
        def write(si: XQueueRequest) = ??? // don't need to write this

        def read(value: JsValue) = value match {
            case jso: JsObject =>
                val xqro = for (
                    JsString(headerS) <- jso.fields.get("xqueue_header");
                    JsString(filesS) <- jso.fields.get("xqueue_files");
                    files <- if (filesS.trim().isEmpty()) { Some(Map.empty[String, String]) } else { Some(filesS.parseJson.convertTo[Map[String, String]]) };
                    JsString(bodyS) <- jso.fields.get("xqueue_body")
                ) yield XQueueRequest(headerS.parseJson.convertTo[XQueueHeader], files, bodyS.parseJson.convertTo[XQueueBody]);
                xqro match {
                    case Some(xqr) => xqr
                    case _         => deserializationError(s"Invalid XQueueRequest object: $value")
                }
            case _ => deserializationError(s"XQueueRequest object expected, but got $value")
        }
    }
    //    implicit object XQResponseJsonFormat extends RootJsonFormat[XQueueResponse] {
    //        def write(xqr: XQueueResponse) = {
    //            val content = Map(
    //                ("xqueue_header" -> JsString(xqr.xqueue_header.toJson.compactPrint)),
    //                ("xqueue_body") -> JsString(xqr.xqueue_body.toJson.compactPrint));
    //            JsObject(content)
    //        }
    //
    //        def read(value: JsValue) = ??? // not necessary
    //    }
}