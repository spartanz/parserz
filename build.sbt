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
      scalaVersion := "2.12.10",
      scalacOptions ++= Seq("-Xsource:2.13"),
      libraryDependencies ++= Seq(
        compilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
        compilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1"),
        "org.scalaz" %% "scalaz-core" % "7.3.0-M30",
        "org.specs2" %% "specs2-core" % "4.6.0" % Test
      )
    )
    .enablePlugins(BuildInfoPlugin)
    .settings(
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoPackage := "spartanz.parserz",
      buildInfoObject := "BuildInfo"
    )
