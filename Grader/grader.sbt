name := "Grader"

organization := "se.kthx"

version := "1.0"

scalaVersion := "2.11.8"

//resolvers += Resolver.mavenLocal

resolvers += "Kompics Releases" at "http://kompics.sics.se/maven/repository/"
resolvers += "Kompics Snapshots" at "http://kompics.sics.se/maven/snapshotrepository/"
resolvers += "Spray Repo" at "http://repo.spray.io"


libraryDependencies += "com.larskroll" %% "common-utils-scala" % "1.2"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.9"
libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.3.9"
libraryDependencies += "com.typesafe.akka" %% "akka-slf4j" % "2.3.9"
libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.3.9" % "test"
libraryDependencies += "io.spray" %% "spray-can" % "1.3.3"
libraryDependencies += "io.spray" %% "spray-routing-shapeless2" % "1.3.3"
libraryDependencies += "io.spray" %% "spray-caching" % "1.3.3"
libraryDependencies += "io.spray" %% "spray-client" % "1.3.3"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.2"
libraryDependencies += "io.spray" %% "spray-testkit" % "1.3.3" % "test"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.1.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.6.0"
libraryDependencies += ("se.kth.edx" %% "kompicsteaching" % "0.9.2-SNAPSHOT").exclude("org.bouncycastle", "bcpg-jdk15on").exclude("org.bouncycastle", "bcprov-jdk15on")


mainClass in assembly := Some("se.kthx.grader.Main")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
  {
    case PathList("com", "google", "common", xs @ _*)         => MergeStrategy.first
    case PathList("com", "typesafe", "config", xs @ _*)         => MergeStrategy.first
    case PathList("javassist", xs @ _*)				=> MergeStrategy.first
    //case PathList("org.bouncycastle", xs @ _*)		=> MergeStrategy.discard
    case "log4j.properties"							=> MergeStrategy.filterDistinctLines
    case "logback.xml"             => MergeStrategy.filterDistinctLines
    case x => old(x)
  }
}

excludedJars in assembly <<= (fullClasspath in assembly) map {
      cp =>
        cp filter {
          c => List(
            "bcpg-jdk15on-1.55.jar",
            "bcprov-jdk15on-1.55.jar"
          ) exists {
            c.data.getName contains _
          }
        }
    }