import sbt._

object Dependencies {

  val enumeratum = "com.beachape" %% "enumeratum" % "1.5.13"

  val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.2" % Test

  val scalaTest = "org.scalatest" %% "scalatest" % "3.1.0" % Test

}