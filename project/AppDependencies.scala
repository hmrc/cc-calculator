import sbt._
import play.core.PlayVersion
import play.sbt.PlayImport._

object AppDependencies {
  
  val compile: Seq[ModuleID] = Seq(
    ws,
    "uk.gov.hmrc"                   %% "bootstrap-backend-play-27"        % "5.3.0",
    "com.github.java-json-tools"    % "json-schema-validator"             % "2.2.14",
    "org.json4s"                    %% "json4s-jackson"                   % "3.6.10",
    "com.typesafe.play"             %% "play-json-joda"                   % "2.9.1"
  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply(): Seq[ModuleID] = new TestDependencies {
      override lazy val test = Seq(
        "org.scalatestplus.play"    %% "scalatestplus-play"     % "4.0.3"                % scope,
        "org.scalatest"             %% "scalatest"              % "3.0.8"                 % scope,
        "org.pegdown"               % "pegdown"                 % "1.6.0"                 % scope,
        "com.typesafe.play"         %% "play-test"              % PlayVersion.current     % scope,
        "org.mockito"               % "mockito-core"            % "3.2.4"                 % scope
      )
    }.test
  }

  def apply(): Seq[ModuleID] = compile ++ Test()
}

