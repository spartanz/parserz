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

lazy val scalaz =
  ProjectRef(
    uri("git:https://github.com/scalaz/scalaz.git#series/8.0.x"),
    "baseJVM"
  )

lazy val root =
  (project in file("."))
    .settings(
      name := "scalaz-parsers",
      resolvers += "Sonatype OSS Snapshots".at(
        "https://oss.sonatype.org/content/repositories/snapshots"
      ),
      libraryDependencies ++= Seq(
//        "org.scalaz" %% "scalaz-base" % "96627337-SNAPSHOT",
        "org.scalaz" %% "scalaz-zio" % "0.2.7"
      )
    )
    .dependsOn(scalaz)
    .enablePlugins(BuildInfoPlugin)
    .settings(
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoPackage := "scalaz.parsers",
      buildInfoObject := "BuildInfo"
    )
