package se.kth.edx.id2203.tutorial

import se.sics.kompics.sl._;
import se.sics.kompics.{ Kompics, Start, Init };
import se.sics.kompics.KompicsEvent;

//-----------------------------------------------------
//STEP 1: Define the basic events fotr the service
case object Ping extends KompicsEvent;
case object Pong extends KompicsEvent;

//-----------------------------------------------------
//STEP 2: Specify the Port that defines the interface for the service
object PingPongPort extends Port {
  request(Ping);
  indication(Pong);
};

//-----------------------------------------------------
//STEP 3: Implement basic service components that run the service

class Pinger extends ComponentDefinition {
  //STEP 3A : Bind service interfaces
  val ppp = requires(PingPongPort);

  //STEP 3B: Define event handlers that implement service logic
  ctrl uponEvent{
    case _: Start => handle {
      print("Ping")
      trigger(Ping, ppp);
    }
  }
  ppp uponEvent{
    case Pong => handle {
      print("Ping")
      trigger(Ping, ppp);
    }
  }
};
class Ponger extends ComponentDefinition{
  //STEP 3A : Bind service interfaces
  val ppn = provides(PingPongPort);

  /*
  Insert missing declaration here
  */
  var num_pings = 0;

  //STEP 3B: Define event handlers that implement service logic 
  ppn uponEvent{
    case Ping => handle{
      /*
      Insert missing handler logic here
      */
      num_pings = num_pings + 1;
      if (num_pings % 3 == 0) println("PONG");

      trigger(Pong, ppn);
    }
  }
};

//-----------------------------------------------------
//STEP 4: Instantiate components and execute the service

class PingService extends ComponentDefinition{
  //STEP 4A: Instantiate Components
  val ponger = create(classOf[Ponger], Init.NONE);
  val pinger = create(classOf[Pinger], Init.NONE);
  connect(PingPongPort)(ponger -> pinger);
};

//STEP 4C: Execute the service
object PingPongApp extends App{
  Kompics.shutdown();
  Kompics.createAndStart(classOf[PingService]);
  try{
    Thread.sleep(200);
  }
  catch{
    case e:InterruptedException => System.exit(1);
  }
  Kompics.shutdown();

}

//uncomment for interpreter run

//PingPongApp.main(Array());
