name := "LocalGrader"

organization := "se.kthx"

version := "1.0"

scalaVersion := "2.13.1"


resolvers += Resolver.mavenLocal
resolvers += Resolver.jcenterRepo
resolvers += Resolver.bintrayRepo("kompics", "Maven")
resolvers += Resolver.bintrayRepo("lkrollcom", "maven")


libraryDependencies += "com.larskroll" %% "common-utils-scala" % "2.1.0"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.5"
libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
libraryDependencies += "org.rogach" %% "scallop" % "3.3.2"
libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.8.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"
libraryDependencies += ("se.kth.edx" %% "kompicsteaching" % "1.2.1").exclude("org.bouncycastle", "bcpg-jdk15on").exclude("org.bouncycastle", "bcprov-jdk15on")


mainClass in assembly := Some("se.kthx.grader.Main")

assemblyMergeStrategy in assembly := { 
    case PathList("com", "google", "common", xs @ _*)         => MergeStrategy.first
    case PathList("com", "typesafe", "config", xs @ _*)         => MergeStrategy.first
    case PathList("javassist", xs @ _*)				=> MergeStrategy.first
    //case PathList("org.bouncycastle", xs @ _*)		=> MergeStrategy.discard
    case "log4j.properties"							=> MergeStrategy.filterDistinctLines
    case "logback.xml"             => MergeStrategy.discard //MergeStrategy.filterDistinctLines
    case x =>
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
}

assemblyExcludedJars in assembly := { 
  val cp = (fullClasspath in assembly).value
  cp filter {
    c => List(
      "bcpg-jdk15on-158.jar",
      "bcprov-jdk15on-158.jar"
    ) exists {
      c.data.getName contains _
    }
  }
}
