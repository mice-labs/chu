import team.mice.Dependencies

lazy val commonSettings = Seq(
  scalaVersion     := "3.3.7",
  organization     := "team.mice",
  organizationName := "Mice Labs",
  versionScheme    := Some("early-semver"),
  dependencyUpdatesFilter -= moduleFilter(name = "scala-library"),
  scalacOptions := {
    scalaBinaryVersion.value match {
      case v if v.startsWith("2.13") => Seq("-Ymacro-annotations", "-Xlint", "-Ywarn-unused", "-deprecation", "")
      case _                         => Seq("-explain", "-Ykind-projector")
    }
  },
  homepage   := Some(url("https://mice-labs.github.io/chu/")),
  licenses   := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt")),
  developers := List(
    Developer(
      id = "kai.matsuda",
      name = "Kai Matsuda",
      email = "kai.matsuda@mice.team",
      url = url("https://vangogh500.github.io/")
    )
  ),
  scmInfo := Some(
    ScmInfo(url("https://github.com/mice-labs/chu"), "scm:git@github.com:mice-labs/chu.git")
  )
)

lazy val publishSettings = Seq(
  publishTo := sonatypePublishToBundle.value,
  credentials += Credentials(
    "Central Repository",
    "central.sonatype.com",
    sys.env.getOrElse("SONATYPE_USERNAME", ""),
    sys.env.getOrElse("SONATYPE_PASSWORD", "")
  ),
  credentials += Credentials(
    "GnuPG Key ID",
    "gpg",
    sys.env.getOrElse("PGP_KEY_ID", ""),
    sys.env.getOrElse("PGP_PASSPHRASE", "")
  ),
  publishMavenStyle      := true,
  Test / publishArtifact := false,
  pomIncludeRepository   := { _ => false }
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
    coverageMinimumStmtTotal   := 75,
    coverageMinimumBranchTotal := 75,
    coverageFailOnMinimum      := true,
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
