val kompicsVersion = "1.2.1"
val kompicsScalaVersion = "2.0.0"


name := "EdxTemplates"

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
libraryDependencies += "se.sics.kompics.basic" % "kompics-component-netty-network" % kompicsVersion
libraryDependencies += "com.michaelpollmeier" %% "scala-arm" % "2.1"
//libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"

//libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "0.9.28" % "test"
libraryDependencies ++= Seq("org.slf4j" % "slf4j-api" % "1.7.5", "org.slf4j" % "slf4j-simple" % "1.7.5")
libraryDependencies += "se.kth.edx" %% "kompicsteaching" % kompicsVersion


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
