package se.kth.edx.id2203.templates

import org.scalatest.{ FunSuite, Matchers };
import se.kth.edx.id2203.validation.checkBLE;

class BLETest extends FunSuite with Matchers {
  test("Gossip Leader election should fulfil BLE properties") {
    checkBallotLE[GossipLeaderElection] should be (true);
  }
}
