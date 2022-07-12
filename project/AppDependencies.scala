import sbt._
import play.core.PlayVersion
import play.sbt.PlayImport._

object AppDependencies {
  
  val compile: Seq[ModuleID] = Seq(
    ws,
    "uk.gov.hmrc"                   %% "bootstrap-backend-play-28"        % "5.24.0",
    "com.github.java-json-tools"    % "json-schema-validator"             % "2.2.14",
    "org.json4s"                    %% "json4s-jackson"                   % "4.0.5",
    "com.typesafe.play"             %% "play-json-joda"                   % "2.9.2",
    "com.fasterxml.jackson.module"  %%  "jackson-module-scala"            % "2.13.3"
  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply(): Seq[ModuleID] = new TestDependencies {
      override lazy val test = Seq(
      "uk.gov.hmrc"                   %% "bootstrap-test-play-28"     % "5.25.0"            % scope,
      "org.mockito"                   %   "mockito-core"              % "4.6.1"             % scope,
      "org.scalatestplus"             %%  "scalatestplus-mockito"     % "1.0.0-M2"          % scope,
      )
    }.test
  }

  def apply(): Seq[ModuleID] = compile ++ Test()
}

