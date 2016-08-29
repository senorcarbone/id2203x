package se.kth.edx.id2203.validation.testers

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.Ports.{AR_Read_Request, AR_Read_Response, AR_Write_Request, AR_Write_Response}

import scala.collection.mutable.ListBuffer

class LinerizationTest extends FlatSpec with Matchers {

  val testOps = Array(
    (toAddress(1), 0l, AR_Write_Request(0)),
    (toAddress(2), 0l, AR_Write_Request(0)),
    (toAddress(3), 2l, AR_Read_Request()),
    (toAddress(4), 3l, AR_Write_Request(2)),
    (toAddress(5), 3l, AR_Read_Request()),
    (toAddress(1), 5l, AR_Write_Response()),
    (toAddress(3), 6l, AR_Read_Response(Some(2))),
    (toAddress(2), 8l, AR_Write_Response()),
    (toAddress(4), 10l, AR_Write_Response()),
    (toAddress(5), 12l, AR_Read_Response(Some(2)))
  )

  val failOps = Array(
    (toAddress(1), 0l, AR_Write_Request(0)),
    (toAddress(2), 0l, AR_Write_Request(0)),
    (toAddress(3), 2l, AR_Read_Request()),
    (toAddress(4), 3l, AR_Write_Request(2)),
    (toAddress(5), 3l, AR_Read_Request()),
    (toAddress(1), 5l, AR_Write_Response()),
    (toAddress(3), 6l, AR_Read_Response(Some(2))),
    (toAddress(6), 7l, AR_Read_Request()),
    (toAddress(2), 8l, AR_Write_Response()),
    (toAddress(4), 10l, AR_Write_Response()),
    (toAddress(5), 12l, AR_Read_Response(Some(2))),
    (toAddress(7), 20l, AR_Read_Response(Some(0)))
  )

  "ABDTester" should "pass scenarios where a linerarization exists" in {
    val valid = ABDTester.validateLinerizability(ListBuffer(testOps: _*));
    valid should be (true);
  }

  "ABDTester" should "fail scenarios where no linerarization exists" in {
    val valid = ABDTester.validateLinerizability(ListBuffer(failOps: _*));
    valid should be (false);
  }
}