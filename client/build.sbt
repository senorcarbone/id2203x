//val kompicsVersion = "1.0.0-SNAPSHOT"
val kompicsVersion = "1.2.1"
val kompicsScalaVersion = "2.0.0"


name := "KompicsTeaching"

organization := "se.kth.edx"

scalaVersion := "2.13.1"

version := kompicsVersion

scalacOptions ++= Seq("-deprecation","-feature")

resolvers += Resolver.mavenLocal
resolvers += Resolver.jcenterRepo
resolvers += Resolver.bintrayRepo("kompics", "Maven")
//resolvers += "Kompics Releases" at "http://kompics.sics.se/maven/repository/"
//resolvers += "Kompics Snapshots" at "http://kompics.sics.se/maven/snapshotrepository/"


libraryDependencies += "se.sics.kompics" % "kompics-core" % kompicsVersion
libraryDependencies += "se.sics.kompics" %% "kompics-scala" % kompicsScalaVersion
libraryDependencies += "se.sics.kompics" %% "kompics-scala-simulator" % kompicsScalaVersion
libraryDependencies += "se.sics.kompics.basic" % "kompics-component-java-timer" % kompicsVersion
libraryDependencies += "se.sics.kompics.basic" % "kompics-port-network" % kompicsVersion
//libraryDependencies += "se.sics.kompics.basic" % "kompics-component-netty-network" % kompicsVersion
libraryDependencies += "org.bouncycastle" % "bcpg-jdk15on" % "1.55"
libraryDependencies += "org.bouncycastle" % "bcprov-jdk15on" % "1.55"
// https://github.com/jsuereth/scala-arm/issues/79
libraryDependencies += "com.michaelpollmeier" %% "scala-arm" % "2.1"
//libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"
libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.8.0"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.5"
//libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.2"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "0.9.28"  % "test"
libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.5"


logBuffered in Test := false
parallelExecution in Test := false


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
