package team.mice

import sbt._

object Dependencies {

  object Cats {
    private val version       = "2.13.0"
    private val effectVersion = "3.6.3"
    val core                  = "org.typelevel" %% "cats-core"        % version
    val effect                = "org.typelevel" %% "cats-effect"      % effectVersion
    val laws                  = "org.typelevel" %% "cats-laws"        % version
    val effectLaws            = "org.typelevel" %% "cats-effect-laws" % effectVersion
  }

  object FS2 {
    private val version = "3.12.0"
    val core            = "co.fs2" %% "fs2-core" % version
  }

  object Http4s {
    private val version = "0.21.15"
    val client          = "org.http4s" %% "http4s-client" % version
    val circe           = "org.http4s" %% "http4s-circe"  % version
  }

  object Circe {
    private val version = "0.14.5"
    val core            = "io.circe" %% "circe-core"    % version
    val generic         = "io.circe" %% "circe-generic" % version
    val parser          = "io.circe" %% "circe-parser"  % version
  }

  object Jsoniter {
    private val version = "2.38.8"
    val core            = "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % version
    val macros          = "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % version % "compile-internal"
  }

  object Scalatest {
    private val version = "3.2.3"
    val core: ModuleID  = "org.scalatest" %% "scalatest" % version
  }

  object ScalaMock {
    val core: ModuleID = "org.scalamock" %% "scalamock" % "5.1.0"
  }

  object Weaver {
    private val version      = "0.11.3"
    val cats: ModuleID       = "org.typelevel" %% "weaver-cats"       % version
    val scalacheck: ModuleID = "org.typelevel" %% "weaver-scalacheck" % version
    val discipline: ModuleID = "org.typelevel" %% "weaver-discipline" % version
  }
}
