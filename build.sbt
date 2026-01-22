import team.mice.Dependencies

lazy val commonSettings = Seq(
  ThisBuild / scalaVersion   := "3.3.7",
  organization               := "team.mice",
  dependencyUpdatesFailBuild := true,
  dependencyUpdatesFilter -= moduleFilter(name = "scala-library")
)

lazy val publishSettings = Seq(
  publishTo             := sonatypePublishTo.value,
  publishConfiguration  := publishConfiguration.value.withOverwrite(isSnapshot.value),
  coverageMinimum       := 90,
  coverageFailOnMinimum := true,
  scalacOptions         := {
    scalaBinaryVersion.value match {
      case v if v.startsWith("2.13") => Seq("-Ymacro-annotations", "-Xlint", "-Ywarn-unused", "-deprecation", "")
      case _                         => Seq("-explain", "-Ykind-projector")
    }
  },
  homepage   := Some(url("https://mice-labs.github.io/cymophane/")),
  licenses   := List("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  developers := List(
    Developer(id = "matsudaskai", name = "Kai Matsuda", email = "", url = url("https://vangogh500.github.io/"))
  ),
  scmInfo := Some(
    ScmInfo(url("https://github.com/mice-labs/cymophane"), "scm:git@github.com:mice-labs/cymophane.git")
  ),
  credentials += Credentials(
    "Sonatype Nexus Repository Manager",
    "oss.sonatype.org",
    sys.env.getOrElse("SONATYPE_USERNAME", ""),
    sys.env.getOrElse("SONATYPE_PASSWORD", "")
  )
)

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true
  )
  .aggregate(
    core
  )

lazy val core = project
  .in(file("core"))
  .enablePlugins(GitVersioning)
  .settings(
    name := "chu-core",
    commonSettings,
    publishSettings,
    libraryDependencies ++= Seq(
      Dependencies.Cats.core,
      Dependencies.FS2.core
    ) ++ Seq(
      Dependencies.Weaver.cats,
      Dependencies.Weaver.discipline,
      Dependencies.Cats.laws,
      Dependencies.Cats.effectLaws
    ).map(_ % "test")
  )
