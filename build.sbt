import spartanz.sbtorgpolicies.Formatting._

inThisBuild(
  List(
    scalafmtOnCompile := false,
    scalafmtGenerateConfigOnLoad := {}
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

inThisBuild(
  List(
    organization := "org.spartanz",
    homepage := Some(url("https://github.com/spartanz/parserz")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/spartanz/parserz"),
        "scm:git:https://github.com/spartanz/parserz.git"
      )
    ),
    developers := List(
      Developer(
        "sergei-shabanau",
        "Sergei Shabanau",
        "serge.shabanau@gmail.com",
        url("https://github.com/sergei-shabanau")
      )
    )
  )
)

lazy val root =
  (project in file("."))
    .settings(
      name := "parserz",
      scalaVersion := "2.13.1",
      crossScalaVersions := Seq("2.12.10", "2.13.1"),
      scalacOptions --= Seq("-Yno-adapted-args", "-Ypartial-unification"),
      libraryDependencies ++= Seq(
        compilerPlugin(("org.typelevel" % "kind-projector" % "0.11.0").cross(CrossVersion.full)),
        compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
        "org.specs2" %% "specs2-core" % "4.8.0" % Test,
        "com.lihaoyi" %% "fastparse" % "2.2.4" % Test,
      )
    )
    .enablePlugins(BuildInfoPlugin)
    .settings(
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoPackage := "spartanz.parserz",
      buildInfoObject := "BuildInfo"
    )
