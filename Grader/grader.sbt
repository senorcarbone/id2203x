name := "Grader"

organization := "se.kthx"

version := "1.0"

scalaVersion := "2.11.8"

//resolvers += Resolver.mavenLocal

resolvers += "Kompics Releases" at "http://kompics.sics.se/maven/repository/"
resolvers += "Kompics Snapshots" at "http://kompics.sics.se/maven/snapshotrepository/"
resolvers += "Spray Repo" at "http://repo.spray.io"


libraryDependencies += "com.larskroll" %% "common-utils-scala" % "1.2"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "0.9.28"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.9"
libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.3.9"
libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % "2.3.9"
libraryDependencies += "io.spray" %% "spray-can" % "1.3.3"
libraryDependencies += "io.spray" %% "spray-routing-shapeless2" % "1.3.3"
libraryDependencies += "io.spray" %% "spray-caching" % "1.3.3"
libraryDependencies += "io.spray" %% "spray-client" % "1.3.3"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.2"
libraryDependencies += "io.spray" %% "spray-testkit" % "1.3.3" % "test"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.1.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.6.0"



mainClass in assembly := Some("se.kthx.grader.Main")