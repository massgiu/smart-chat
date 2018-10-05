name := "smart-chat"

version := "0.1"

scalaVersion := "2.12.7"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies +=
  "com.typesafe.akka" %% "akka-actor" % "2.5.16"

libraryDependencies +=
  "com.typesafe.akka" %% "akka-remote" % "2.5.16"

libraryDependencies +=
  "com.typesafe.akka" %% "akka-testkit" % "2.5.17" % Test

libraryDependencies +=
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"