import sbt._

object Libs {
  val `scalatest` = "org.scalatest" %% "scalatest" % "3.0.1"
  val `jmdns` = "org.jmdns" % "jmdns" % "3.5.1"
  val `scala-java8-compat` = "org.scala-lang.modules" %% "scala-java8-compat" % "0.8.0"
  val `scalamock-scalatest-support` = "org.scalamock" %% "scalamock-scalatest-support" % "3.5.0"
  val `scala-async` = "org.scala-lang.modules" %% "scala-async" % "0.9.6"
  val `scopt` = "com.github.scopt" %% "scopt" % "3.5.0"
  val `scala-logging` = "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"
  val `acyclic` = "com.lihaoyi" %% "acyclic" % "0.1.7" % "provided"
  val `enumeratum` = "com.beachape" %% "enumeratum" % "1.5.8"
  val `junit` = "junit" % "junit" % "4.12"
  val `junit-interface` = "com.novocode" % "junit-interface" % "0.11"
}

object Akka {
  val Version = "2.4.17"
  val `akka-actor` = "com.typesafe.akka" %% "akka-actor" % Version
  val `akka-remote` = "com.typesafe.akka" %% "akka-remote" % Version
  val `akka-stream` = "com.typesafe.akka" %% "akka-stream" % Version
  val `akka-stream-testkit` = "com.typesafe.akka" %% "akka-stream-testkit" % Version
  val `akka-distributed-data-experimental` = "com.typesafe.akka" %% "akka-distributed-data-experimental" % Version
}