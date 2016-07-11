name := "BloomFilterTree"
scalaVersion := "2.11.8"
organization := "org.janzhou"

enablePlugins(GitVersioning)

scalacOptions ++= Seq("-optimise", "-feature", "-deprecation")

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % "0.12"
)

resolvers += Resolver.jcenterRepo
