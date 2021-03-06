name := "golem"

version := "0.1"

scalaVersion := "2.10.2"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-actor" % "2.2.3",
                            "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
                            "com.typesafe.akka" %% "akka-testkit" % "2.2.3")

