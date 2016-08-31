package se.kthx

import spray.client.pipelining._
import spray.http._

package object grader {
    def forceContentType(contentType: ContentType): ResponseTransformer = {
        response =>
            {
                response.withEntity(response.entity.flatMap(_.copy(contentType = contentType)))
            }
    }
}