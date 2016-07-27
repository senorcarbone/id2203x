package se.kth.edx.id2203.tutorial


import se.sics.kompics.KompicsEvent
import se.sics.kompics.sl._

//-----------------------------------------------------
//STEP 1: Define the basic events for the service
case class Ping() extends KompicsEvent;

case class Pong() extends KompicsEvent;

//-----------------------------------------------------
//STEP 2: Specify the Port that defines the interface for the service
object PingPongPort extends Port {
  request[Ping];
  indication[Pong];
};

//-----------------------------------------------------
//STEP 3: Implement basic service components that run the service

class Pinger extends ComponentDefinition {
  //STEP 3A : Bind service interfaces
  val ppp = requires(PingPongPort);

  //STEP 3B: Define event handlers that implement service logic
  ctrl uponEvent {
    case _: Start => handle {
      trigger(Ping(), ppp);
    }
  }
  ppp uponEvent {
    case Pong() => handle {
      println("Got a Pong!");
      trigger(Ping(), ppp);
    }
  }
};

class Ponger extends ComponentDefinition {
  //STEP 3A : Bind service interfaces
  val ppn = provides(PingPongPort);

  //STEP 3B: Define event handlers that implement service logic 
  ppn uponEvent {
    case Ping() => handle {
      println("Got a Ping!");
      trigger(Pong(), ppn);
    }
  }
};

//-----------------------------------------------------
//STEP 4: Instantiate components and execute the service

class PingService extends ComponentDefinition {
  //STEP 4A: Instantiate Components
  val ponger = create(classOf[Ponger], se.sics.kompics.Init.NONE);
  val pinger = create(classOf[Pinger], se.sics.kompics.Init.NONE);
  connect(PingPongPort)(ponger -> pinger);
};

//STEP 4C: Execute the service
object PingPongApp extends App {
  Kompics.shutdown();
  Kompics.createAndStart(classOf[PingService]);
  try {
    Thread.sleep(5000);
  }
  catch {
    case e: InterruptedException => System.exit(1);
  }
  Kompics.shutdown();
}

PingPongApp main Array()

