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
      resolvers += "Sonatype OSS Staging".at(
        "https://oss.sonatype.org/content/repositories/staging"
      ),
      libraryDependencies ++= Seq(
        "org.scalaz" %% "scalaz-zio"  % "0.2.9",
        "org.scalaz" %% "scalaz-base" % "96627337-SNAPSHOT",
        "org.specs2" %% "specs2-core" % "4.3.4" % Test
      ),
      scalacOptions in Test ++= Seq("-Yrangepos")
    )
    .enablePlugins(BuildInfoPlugin)
    .settings(
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoPackage := "scalaz.parsers",
      buildInfoObject := "BuildInfo"
    )
