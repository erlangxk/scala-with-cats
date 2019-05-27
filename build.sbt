scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
	"org.typelevel" %% "cats-core" % "1.6.0"
)
	

scalacOptions ++= Seq(
	"-Xfatal-warnings",
	"-Ypartial-unification"
)