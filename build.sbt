
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

lazy val root =
  (project in file("."))
    .settings(
      name := "scalaz-parsers",
      resolvers += "Sonatype OSS Snapshots".at(
        "https://oss.sonatype.org/content/repositories/snapshots"
      ),
      libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-base" % "8.0.0-SNAPSHOT",
        "org.scalaz" %% "scalaz-zio"  % "0.1-SNAPSHOT"
      )
    )
    .enablePlugins(BuildInfoPlugin)
    .settings(
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoPackage := "scalaz.parsers",
      buildInfoObject := "BuildInfo"
    )
