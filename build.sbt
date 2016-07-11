name := "BloomFilterTree"
scalaVersion := "2.11.8"
organization := "org.janzhou"

enablePlugins(GitVersioning)

scalacOptions ++= Seq("-feature", "-deprecation")

crossScalaVersions := Seq("2.11.8", "2.12.0-M4")

publishTo := Some(Resolver.file("file", new File(Path.userHome.absolutePath+"/projects/mvn-repo")))
