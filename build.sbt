organization := "io.nary"

name := "condenser"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "bintray/rossabaker" at "http://dl.bintray.com/rossabaker/maven"
)

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.8.0" cross CrossVersion.binary)

mainClass in Compile := Some("io.nary.condenser.operation.Main")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.15",
  "org.scalaz.stream" %% "scalaz-stream" % "0.8a",
  "org.tpolecat" %% "atto-core"  % "0.4.2",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.http4s" %% "http4s-dsl" % "0.16.4a",
  "org.http4s" %% "http4s-blaze-server" % "0.16.4a",
  "org.http4s" %% "http4s-blaze-client" % "0.16.4a",
  "org.http4s" %% "http4s-argonaut" % "0.16.4a",
  "org.spire-math" %% "jawn-argonaut" % "0.11.0",
  "org.spire-math" %% "jawn-parser" % "0.11.0",
  "org.spire-math" %% "jawn-ast" % "0.11.0",
  "io.argonaut" %% "argonaut" % "6.2",
  "org.typelevel" %% "squants" % "1.3.0"
  )
