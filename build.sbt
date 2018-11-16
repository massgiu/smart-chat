name := "smart-chat"

version := "0.1"

scalaVersion := "2.12.7"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
resolvers += "jitpack" at "https://jitpack.io"

libraryDependencies +=
  "com.typesafe.akka" %% "akka-actor" % "2.5.16"

libraryDependencies +=
  "com.typesafe.akka" %% "akka-remote" % "2.5.16"

libraryDependencies +=
  "com.typesafe.akka" %% "akka-testkit" % "2.5.17" % Test

libraryDependencies +=
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies +=
  "com.github.gscaparrotti" % "Bubble" % "628c8213bb"