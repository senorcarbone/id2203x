val kompicsVersion = "0.9.2-SNAPSHOT"


name := "KompicsTeaching"

organization := "se.kth.edx"

scalaVersion := "2.11.8"

version := kompicsVersion

scalacOptions ++= Seq("-deprecation","-feature")

resolvers += Resolver.mavenLocal
resolvers += "Kompics Releases" at "http://kompics.sics.se/maven/repository/"
resolvers += "Kompics Snapshots" at "http://kompics.sics.se/maven/snapshotrepository/"


libraryDependencies += "se.sics.kompics" % "kompics-core" % kompicsVersion
libraryDependencies += "se.sics.kompics" % "kompics-scala_2.11" % kompicsVersion
libraryDependencies += "se.sics.kompics" % "kompics-scala-simulator_2.11" % kompicsVersion
libraryDependencies += "se.sics.kompics.basic" % "kompics-component-java-timer" % kompicsVersion
libraryDependencies += "se.sics.kompics.basic" % "kompics-port-network" % kompicsVersion
//libraryDependencies += "se.sics.kompics.basic" % "kompics-component-netty-network" % kompicsVersion
libraryDependencies += "org.bouncycastle" % "bcpg-jdk15on" % "1.55"
libraryDependencies += "org.bouncycastle" % "bcprov-jdk15on" % "1.55"
libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"
libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.6.0"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.2"
//libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.2"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "0.9.28"  % "test"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.5"





publishMavenStyle := true
//credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

publishTo <<= version { (v: String) =>
  val kompics = "kompics.i.sics.se";
  val keyFile = Path.userHome / ".ssh" / "id_rsa";
  if (v.trim.endsWith("SNAPSHOT"))
    Some(Resolver.sftp("SICS Snapshot Repository", kompics, "/home/maven/snapshotrepository") as("root", keyFile))
  else
    Some(Resolver.sftp("SICS Release Repository", kompics, "/home/maven/repository") as("root", keyFile))
}
