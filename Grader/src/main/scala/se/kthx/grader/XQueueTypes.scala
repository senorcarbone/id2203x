package se.kthx.grader

import spray.json._

case class XQueueResponse(correct: Boolean, score: Int, msg: String)
case class XQueueRequest(xqueue_header: XQueueHeader, xqueue_files: Map[String, String], xqueue_body: XQueueBody)
case class XQueueHeader(submission_id: Long, submission_key: String)
case class XQueueBody(student_info: StudentInfo, student_response: String, grader_payload: String)
case class StudentInfo(anonymous_student_id: String, submission_time: String, random_seed: Long)

object XQueueJsonProtocol extends DefaultJsonProtocol {
    implicit val responseFormat = jsonFormat3(XQueueResponse);
    implicit val infoFormat = jsonFormat3(StudentInfo);
    implicit val headerFormat = jsonFormat2(XQueueHeader);
    implicit val bodyFormat = jsonFormat3(XQueueBody);
    implicit val requestFormat = jsonFormat3(XQueueRequest);
}