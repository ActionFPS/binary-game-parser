scalaVersion in ThisBuild := "2.11.8"
scalacOptions in ThisBuild := Seq(
  "-unchecked", "-deprecation", "-encoding", "utf8", "-feature",
  "-language:existentials", "-language:implicitConversions",
  "-language:reflectiveCalls", "-target:jvm-1.8"
)

libraryDependencies in ThisBuild += "joda-time" % "joda-time" % "2.9.6"
libraryDependencies in ThisBuild += "org.joda" % "joda-convert" % "1.8.1"
javaOptions in ThisBuild += "-Duser.timezone=UTC"
javaOptions in run in ThisBuild += "-Duser.timezone=UTC"
enablePlugins(GitVersioning)
git.useGitDescribe in ThisBuild := true
fork in ThisBuild := true
organization in ThisBuild := "com.actionfps"
crossScalaVersions in ThisBuild := Seq("2.11.8", "2.12.1")
bintrayVcsUrl in ThisBuild := Some("git@github.com:ActionFPS/server-pinger.git")
licenses in ThisBuild += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

  libraryDependencies in ThisBuild += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

val commonsIo: ModuleID = "commons-io" % "commons-io" % "2.5"
val json4s: ModuleID = "org.json4s" %% "json4s-jackson" % "3.4.2"
def akka(stuff: String) = "com.typesafe.akka" %% s"akka-$stuff" % "2.4.14"
val akkaActor: ModuleID = akka("actor")
lazy val pureGame = Project(id = "pure-game", base = file("pure-game"))

lazy val root = project
  .in(file("."))
  .aggregate(demoParser, liveListener)
  .settings(publish := {})
lazy val demoParser =
Project(
  id = "demo-parser",
  base = file("demo-parser")
)
.settings(
  libraryDependencies ++= Seq(
    commonsIo,
    json4s,
    akkaActor
  ),
  git.useGitDescribe := true
)

lazy val liveListener =
  Project(
    id = "live-listener",
    base = file("live-listener")
  )
    .dependsOn(demoParser)
