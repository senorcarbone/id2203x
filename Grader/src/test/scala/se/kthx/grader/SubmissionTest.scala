package se.kthx.grader

import org.scalatest._
import akka.actor._
import akka.testkit._
import se.kth.edx.id2203.validation.Encryptor
import spray.json._
import se.kth.edx.id2203.validation.ValidationUtils.ValidationReport
import se.kth.edx.id2203.validation.ValidationUtils.ValidationSimulation.reportFormat

class SubmissionTest extends TestKit(ActorSystem("testSystem")) with FlatSpecLike with Matchers {
    val hwFileS = Thread.currentThread().getContextClassLoader.getResourceAsStream("testhw.json");
    val hwFile = scala.io.Source.fromInputStream(hwFileS).mkString
    val actorRef = TestActorRef[SubmissionChecker](Props(classOf[SubmissionChecker], null, null))
    val sc = actorRef.underlyingActor

    "SubmissionChecker" should "find tokens" in {

        val tokens = sc.extractTokens(hwFile);
        tokens should not be empty;
        tokens should have size 1;
    }

    it should "decode tokens" in {
        val tokens = sc.extractTokens(hwFile);
        tokens map { token =>
            val report = sc.decrypt(token);
            print(report)
            report should not be (null);
        }
    }

    it should "accumulate points" in {
        val tokens = sc.extractTokens(hwFile);
        tokens should not be empty;
        val reports = tokens.map(sc.decrypt);
        reports should not be empty;
        val result = sc.grade(reports, Set("Eventually Perfect Failure Detector"))
        result should not be (null);
        result._1 should be (1);
    }
}