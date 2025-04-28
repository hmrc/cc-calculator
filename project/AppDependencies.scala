import sbt._
import play.sbt.PlayImport._

object AppDependencies {

  private val bootstrapVersion = "9.7.0"

  val compile: Seq[ModuleID] = Seq(
    ws,
    "uk.gov.hmrc"                  %% "bootstrap-backend-play-30" % bootstrapVersion,
    "com.github.java-json-tools"    % "json-schema-validator"     % "2.2.14",
    "org.json4s"                   %% "json4s-jackson"            % "4.1.0-M4",
    "com.fasterxml.jackson.module" %% "jackson-module-scala"      % "2.16.1"
  )

  val test: Seq[ModuleID] = Seq(
    "uk.gov.hmrc" %% "bootstrap-test-play-30" % bootstrapVersion % Test
  )

  val all: Seq[ModuleID] = compile ++ test
}
