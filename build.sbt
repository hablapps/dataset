name := "dataset"

scalaVersion := "2.11.8"

scalaOrganization := "org.typelevel"

scalaBinaryVersion := "2.11"

ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }

organization := "org.hablapps"

version := "0.1-SNAPSHOT"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % "1.6.1",
  // "org.apache.flink" %% "flink-scala" % "1.2.0",
  // "org.apache.flink" %% "flink-clients" % "1.2.0",
  "org.scalaz" %% "scalaz-scalacheck-binding" % "7.2.7",
  "org.scalatest" %% "scalatest" % "3.0.0",
  "com.chuusai" %% "shapeless" % "2.3.2"
)

resourceDirectory in Test := baseDirectory.value / "resources"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-Ypartial-unification",
  // "-Xprint:typer",
  // "-Xlog-implicit-conversions",
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-language:higherKinds")
