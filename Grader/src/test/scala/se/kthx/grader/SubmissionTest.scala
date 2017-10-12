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
  val hwFile = scala.io.Source.fromInputStream(hwFileS).mkString;
  val hwFileFailS = Thread.currentThread().getContextClassLoader.getResourceAsStream("testhwfail.json");
  val hwFileFail = scala.io.Source.fromInputStream(hwFileFailS).mkString;
  val actorRef = TestActorRef[SubmissionChecker](Props(classOf[SubmissionChecker], null, null));
  val sc = actorRef.underlyingActor;

  "JSON parser" should "deal with angular objects" in {
    val input = """
{"angularObjects":{"2BKQCVH92:shared_process":[],"2CVXXPNWV:shared_process":[]}}
""".stripLineEnd;
    val jdata = input.parseJson.asJsObject
    jdata should not be null
    jdata shouldBe a[JsObject]
  }

  ignore should "deal with formatted json" in {
    val hwFileS2 = Thread.currentThread().getContextClassLoader.getResourceAsStream("testhw2.json");
    val hwFile2 = scala.io.Source.fromInputStream(hwFileS2).mkString;
    val input = """{"test": "lala"}""";
    val diff = hwFile2.diff(input);
    println(s"Difference between '${input}' and '${hwFile2}':\n'${diff}' of length ${diff.length()} as byte ${diff.charAt(0).toInt}");
    val jdata = hwFile2.substring(1).parseJson.asJsObject
    jdata should not be null
    jdata shouldBe a[JsObject]
  }

  "SubmissionChecker" should "deal with failed submissions" in {
    hwFileFail should not be empty;
    val tokens = sc.extractTokens(hwFileFail);
    tokens shouldBe empty;
  }

  it should "find tokens" in {
    val tokens = sc.extractTokens(hwFile);
    tokens should not be empty;
    tokens should have size 1;
  }

  it should "decode tokens" in {
    val tokens = sc.extractTokens(hwFile);
    tokens should not be empty;
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
    println(result._2)
  }

}
