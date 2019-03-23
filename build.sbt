addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

inThisBuild(
  List(
    organization := "org.scalaz",
    homepage := Some(url("https://github.com/scalaz/scalaz-parsers")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List()
  )
)

lazy val root =
  (project in file("."))
    .settings(
      name := "scalaz-parsers",
      scalaVersion := "2.12.8",
      scalacOptions ++= Seq("-Xsource:2.13"),
      resolvers += "Sonatype OSS Staging".at(
        "https://oss.sonatype.org/content/repositories/staging"
      ),
      libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-core" % "7.3.0-M28",
        "org.specs2" %% "specs2-core" % "4.3.5" % Test
      )
    )
    .enablePlugins(BuildInfoPlugin)
    .settings(
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoPackage := "scalaz.parsers",
      buildInfoObject := "BuildInfo"
    )
