import sbt._
import play.core.PlayVersion
import play.sbt.PlayImport._

object AppDependencies {
  
  val compile: Seq[ModuleID] = Seq(
    ws,
    "uk.gov.hmrc"                   %% "bootstrap-backend-play-28"        % "5.3.0",
    "com.github.java-json-tools"    % "json-schema-validator"             % "2.2.14",
    "org.json4s"                    %% "json4s-jackson"                   % "3.6.10",
    "com.typesafe.play"             %% "play-json-joda"                   % "2.9.1",
    "com.fasterxml.jackson.module"  %%  "jackson-module-scala"            % "2.12.2"
  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply(): Seq[ModuleID] = new TestDependencies {
      override lazy val test = Seq(
      "org.mockito"                   %   "mockito-core"              % "3.7.7"             % scope,
      "com.vladsch.flexmark"          %   "flexmark-all"              % "0.35.10"           % scope,
      "org.scalatestplus"             %%  "scalatestplus-mockito"     % "1.0.0-M2"          % scope,
      "org.scalatestplus.play"        %%  "scalatestplus-play"        % "5.1.0"             % scope
      )
    }.test
  }

  def apply(): Seq[ModuleID] = compile ++ Test()
}

