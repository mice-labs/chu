package team.mice

import sbt._

object Dependencies {

  object Cats {
    private val version       = "2.13.0"
    val core                  = "org.typelevel" %% "cats-core"        % version
    val laws                  = "org.typelevel" %% "cats-laws"        % version
  }

  object Weaver {
    private val version      = "0.11.3"
    val cats: ModuleID       = "org.typelevel" %% "weaver-cats"       % version
    val discipline: ModuleID = "org.typelevel" %% "weaver-discipline" % version
  }
}
