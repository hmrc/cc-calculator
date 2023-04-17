import sbt._
import play.core.PlayVersion
import play.sbt.PlayImport._

object AppDependencies {

  val bootstrapVersion = "7.15.0"

  val compile: Seq[ModuleID] = Seq(
    ws,
    "uk.gov.hmrc"                   %% "bootstrap-backend-play-28"        % bootstrapVersion,
    "com.github.java-json-tools"    % "json-schema-validator"             % "2.2.14",
    "org.json4s"                    %% "json4s-jackson"                   % "4.1.0-M2",
    "com.typesafe.play"             %% "play-json-joda"                   % "2.9.4",
    "com.fasterxml.jackson.module"  %% "jackson-module-scala"             % "2.14.2"
  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply(): Seq[ModuleID] = new TestDependencies {
      override lazy val test = Seq(
      "uk.gov.hmrc"                   %% "bootstrap-test-play-28"     % bootstrapVersion    % scope,
      "org.mockito"                   %  "mockito-core"               % "5.2.0"             % scope,
      "org.scalatestplus"             %% "mockito-3-4"                % "3.2.1.0"           % scope
      )
    }.test
  }

  def apply(): Seq[ModuleID] = compile ++ Test()
}

