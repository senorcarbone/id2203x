package se.kthx.grader

import spray.http._
import spray.http.HttpHeaders._
import spray.util._
import spray.http.HttpCharsets._
import spray.httpx.encoding._
import spray.httpx.marshalling._
import spray.httpx.unmarshalling._
import spray.client.pipelining._
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{ Success, Failure }
import spray.httpx.ResponseTransformation

class Session(basePipeline: SendReceive, authData: MultipartFormData)(implicit ec: ExecutionContext) {

    val cookieStore = scala.collection.mutable.Set.empty[HttpCookie];

    def apply(req: HttpRequest): Future[HttpResponse] = {
        val reqC = req ~> addCookies;
        //println(s"Launching session request $reqC")
        val resp = basePipeline(reqC);
        resp.map(handleResponse)
    }

    def handleResponse(r: HttpResponse): HttpResponse = {
        val cookies = r.headers flatMap {
            case h: `Set-Cookie` => Some(h.cookie)
            case _               => None
        }
        cookieStore ++= cookies;
        r
    }

    def addCookies = addHeaders(Cookie(cookieStore.toSeq))
}