package se.kth.edx.id2203.templates

import se.sics.kompics.sl._
import se.sics.kompics.network._
import se.kth.edx.id2203.core.Ports._;
import se.sics.kompics.{ KompicsEvent };
import collection.mutable

object SequencePaxos {
  case class Prepare(nL: Long, ld: Int, na: Long) extends KompicsEvent;
  case class Promise(nL: Long, na: Long, suffix: List[RSM_Command], ld: Int) extends KompicsEvent;
  case class AcceptSync(nL: Long, suffix: List[RSM_Command], ld: Int) extends KompicsEvent;
  case class Accept(nL: Long, c: RSM_Command) extends KompicsEvent;
  case class Accepted(nL: Long, m: Int) extends KompicsEvent;
  case class Decide(ld: Int, nL: Long) extends KompicsEvent;

  object State extends Enumeration {
    type State = Value;
    val PREPARE, ACCEPT, UNKOWN = Value;
  }

  object Role extends Enumeration {
    type Role = Value;
    val LEADER, FOLLOWER = Value;
  }
}

class SequencePaxos(init: Init[SequencePaxos]) extends ComponentDefinition {
  import SequencePaxos._
  import State._
  import Role._

  val sc = provides(SequenceConsensus);
  val ble = requires(BallotLeaderElection);
  val pl = requires(FIFOPerfectLink);

  val (self, pi, others) = init match {
    case Init(addr: Address, pi: Set[Address] @unchecked) => (addr, pi, pi - addr)
  }
  val majority = Math.ceil(pi.size.doubleValue() / 2.0 + 1.0).intValue();

  var state = (FOLLOWER, UNKOWN);
  var nL = 0l;
  var nProm = 0l;
  var leader: Option[Address] = None;
  var na = 0l;
  var va = List.empty[RSM_Command];
  var ld = 0;
  // leader state
  var propCmds = List.empty[RSM_Command];
  val las = mutable.Map.empty[Address, Int];
  val lds = mutable.Map.empty[Address, Int];
  var lc = 0;
  val acks = mutable.Map.empty[Address, (Long, List[RSM_Command])];

  private def suffix(s: List[RSM_Command], l: Int): List[RSM_Command] = {
    s.drop(l)
  }

  private def prefix(s: List[RSM_Command], l: Int): List[RSM_Command] = {
    s.take(l)
  }

  ble uponEvent {
    case BLE_Leader(l, n) => handle {
      if (n > nL) {
        leader = Some(l);
        nL = n;
        if ((self == l) && (nL > nProm)) {
          state = (LEADER, PREPARE);
          propCmds = List.empty;
          las.clear();
          lds.clear();
          acks.clear();
          lc = 0;
          others.foreach { p =>
            trigger (PL_Send(p, Prepare(nL, ld, na)) -> pl);
          };
          acks += (self -> (na -> suffix(va, ld)));
          lds += (self -> ld);
          nProm = nL;
        } else {
          state = (FOLLOWER, state._2);
        }
      } else {
        logger.debug("Leader {} with ballot {} is already outdated!", l, n);
      }
    }
  }

  pl uponEvent {
    case PL_Deliver(p, Prepare(np, ldp, n)) => handle {
      if (nProm < np) {
        nProm = np;
        state = (FOLLOWER, PREPARE);
        val sfx = if (na >= n) suffix(va, ldp) else List.empty;
        trigger (PL_Send(p, Promise(np, na, sfx, ld)) -> pl);
      } else {
        logger.debug("Outdated ({} >= {}) Prepare received from {}", Array(nProm, np, p));
      }
    }
    case PL_Deliver(a, Promise(n, na, sfxa, lda)) => handle {
      if ((n == nL) && (state == (LEADER, PREPARE))) {
        acks += (a -> (na -> sfxa));
        lds += (a -> lda);
        val p = acks.keySet;
        if (p.size == majority) {
          val (k, sfx) = acks.values.foldLeft((0l, List.empty[RSM_Command])) {
            case ((acck, accSfx), (nk, sfxk)) => if ((nk > acck) || ((nk == acck) && (sfxk.size > accSfx.size))) (nk, sfxk) else (acck, accSfx)
          };
          va = prefix(va, ld) ++ sfx ++ propCmds;
          las += (self -> va.size);
          propCmds = List.empty;
          state = (LEADER, ACCEPT);
          others.filter(lds.contains(_)).foreach { p =>
            val ldsp = lds(p);
            trigger (PL_Send(p, AcceptSync(nL, suffix(va, ldsp), ldsp)) -> pl);
          }
        }
      } else if ((n == nL) && (state == (LEADER, ACCEPT))) {
        lds += (a -> lda);
        trigger (PL_Send(a, AcceptSync(nL, suffix(va, lda), lda)) -> pl);
        if (lc != 0) {
          trigger (PL_Send(a, Decide(ld, nL)) -> pl)
        }
      }
    }
    case PL_Deliver(p, AcceptSync(nL, sfx, ldp)) => handle {
      if ((nProm == nL) && (state == (FOLLOWER, PREPARE))) {
        na = nL;
        va = prefix(va, ldp) ++ sfx;
        trigger (PL_Send(p, Accepted(nL, va.size)) -> pl);
        state = (FOLLOWER, ACCEPT);
      }
    }
    case PL_Deliver(p, Accept(nL, c)) => handle {
      if ((nProm == nL) && (state == (FOLLOWER, ACCEPT))) {
        va = va ++ List(c);
        trigger (PL_Send(p, Accepted(nL, va.size)) -> pl);
      }
    }
    case PL_Deliver(_, Decide(l, nL)) => handle {
      if (nProm == nL) {
        while (ld < l) {
          trigger (SC_Decide(va(ld)) -> sc);
          ld = ld + 1;
        }
      }
    }
    case PL_Deliver(a, Accepted(n, m)) => handle {
      if ((n == nL) && (state == (LEADER, ACCEPT))) {
        las += (a -> m);
        val supm = las.filter(t => t._2 >= m).size;
        if ((lc < m) && (supm >= majority)) {
          lc = m;
          lds.keySet.foreach { p =>
            trigger (PL_Send(p, Decide(lc, nL)) -> pl);
          }
        }
      }
    }
  }

  sc uponEvent {
    case SC_Propose(c) => handle {
      if (state == (LEADER, PREPARE)) {
        propCmds = propCmds ++ List(c);
      } else if (state == (LEADER, ACCEPT)) {
        va = va ++ List(c);
        las += (self -> (las(self) + 1));
        lds.keySet.foreach { p =>
          if (p != self) {
            trigger (PL_Send(p, Accept(nL, c)) -> pl);
          }
        }
      }
    }
  }
}
