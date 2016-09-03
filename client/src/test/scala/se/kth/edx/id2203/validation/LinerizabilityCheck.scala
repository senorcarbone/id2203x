package se.kth.edx.id2203.validation.testers

import org.scalatest._
import se.kth.edx.id2203.core.ExercisePrimitives.AddressUtils._
import se.kth.edx.id2203.core.Ports.{AR_Read_Request, AR_Read_Response, AR_Write_Request, AR_Write_Response}

import scala.collection.mutable.ListBuffer

class LinerizabilityCheck extends FlatSpec with Matchers {

  /*     0   1   2   3   4   5   6   7   8   9   10   11   12   13   14
  OP1 :  ---------W0----------
  OP2 :  ---------------W0----------------
  OP3 :          --------R2-------
  OP4 :              --------------W2--------------
  OP5 :              -------------------R2-------------------
 
  Op6  :                                                              ----------------r0-----------
  
    /*     0   1   2   3   4   5   6   7   8   9   10   11   12   13   14
  OP1 :  ---------W0---------- ---------R1----------
  OP2 :       ---------R1---------- ---------W0----------
                              */
 TODO check edge removal matching 
   */


  val cyclicOps = Array(
    (toAddress(1), 0l, AR_Write_Request(Some(0))),
    (toAddress(1), 4l, AR_Write_Response()),
    (toAddress(2), 1l, AR_Read_Request()),
    (toAddress(2), 5l, AR_Read_Response(Some(1))),
    (toAddress(1), 6l, AR_Read_Request()),
    (toAddress(1), 10l, AR_Read_Response(Some(1))),
    (toAddress(2), 7l, AR_Write_Request(Some(0))),
    (toAddress(2), 11l, AR_Write_Response())
  )
  
  
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
    (toAddress(3), 5l, AR_Read_Response(Some(2))),
    (toAddress(6), 14l, AR_Read_Request()),
    (toAddress(2), 8l, AR_Write_Response()),
    (toAddress(4), 10l, AR_Write_Response()),
    (toAddress(5), 12l, AR_Read_Response(Some(2))),
    (toAddress(6), 20l, AR_Read_Response(Some(1)))
  )

  val failOps2 = Array(
    (toAddress(1), 0l, AR_Write_Request(0)),
    (toAddress(2), 0l, AR_Write_Request(0)),
    (toAddress(3), 2l, AR_Read_Request()),
    (toAddress(4), 3l, AR_Write_Request(2)),
    (toAddress(5), 3l, AR_Read_Request()),
    (toAddress(1), 5l, AR_Write_Response()),
    (toAddress(3), 5l, AR_Read_Response(Some(2))),
    (toAddress(6), 14l, AR_Read_Request()),
    (toAddress(2), 8l, AR_Write_Response()),
    (toAddress(4), 10l, AR_Write_Response()),
    (toAddress(5), 12l, AR_Read_Response(Some(2))),
    (toAddress(6), 20l, AR_Read_Response(Some(0))),
    (toAddress(7), 21l, AR_Read_Request()),
    (toAddress(6), 22l, AR_Read_Response(Some(2)))
  )

  "ABDTester" should "pass scenarios where a linerarization exists" in {
    val valid = ABDTester.validateLinerizability(ListBuffer(testOps: _*));
    valid should be (true);
  }

  "ABDTester" should "fail scenarios where no linerarization exists" in {
    val valid = ABDTester.validateLinerizability(ListBuffer(failOps: _*));
    valid should be (false);
  }

  "ABDTester" should "fail scenarios where no linerarization exists between subsequent reads" in {
    val valid = ABDTester.validateLinerizability(ListBuffer(failOps2: _*));
    valid should be (false);
  }

  "ABDTester" should "fail scenarios where there are cycles" in {
    val valid = ABDTester.validateLinerizability(ListBuffer(cyclicOps: _*));
    valid should be (false);
  }
}