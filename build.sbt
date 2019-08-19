scalaVersion := "2.12.9"

libraryDependencies ++= Seq(
	"org.typelevel" %% "cats-effect" % "2.0.0-RC1",
	"io.spray" %% "spray-json" % "1.3.5",
	"com.typesafe.akka" %% "akka-actor" % "2.5.24",
	"com.typesafe.akka" %% "akka-actor-typed" % "2.5.24",
	"ch.qos.logback" % "logback-classic" % "1.2.3",
	"com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
)
	

scalacOptions ++= Seq(
	"-Xfatal-warnings",
	"-Ypartial-unification",
	"-feature"
)