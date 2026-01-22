package team.mice

import sbt._

object Plugins {
  object SBTUpdates {
    private val version = "0.4.2"
    val core: ModuleID = "com.timushev.sbt" % "sbt-updates" % version
  }
  object SCoverage {
    private val version = "1.6.1"
    val core: ModuleID = "org.scoverage" % "sbt-scoverage" % version
  }
  object ScalaFmt {
    private val version = "2.0.1"
    val core: ModuleID = "org.scalameta" % "sbt-scalafmt" % version
  }
  object SBTGit {
    private val version = "2.1.0"
    val core            = "com.github.sbt" % "sbt-git" % version
  }
  object Sonatype {
    private val version = "3.8"
    val core: ModuleID = "org.xerial.sbt" % "sbt-sonatype" % version
  }
  object PGP {
    private val version = "2.0.0"
    val core: ModuleID = "com.jsuereth" % "sbt-pgp" % version
  }
  object Jmh {
    private val version = "0.4.8"
    val core: ModuleID = "pl.project13.scala" % "sbt-jmh" % version
  }
}