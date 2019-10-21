name := "simpleimperative-algebraic-scala"

version := "0.2"

scalaVersion := "2.12.10"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:higherKinds",
  "-Ypartial-unification"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10")

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core"           % "7.2.29",
  "com.slamdata"   %% "matryoshka-core"       % "0.21.3",
  "com.slamdata"   %% "matryoshka-scalacheck" % "0.21.3" % Test,
  "org.scalatest"  %% "scalatest"             % "3.0.8"  % Test
)