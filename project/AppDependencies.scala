import sbt._
import play.sbt.PlayImport._

object AppDependencies {

  val bootstrapVersion = "9.0.0"

  val compile: Seq[ModuleID] = Seq(
    ws,
    "uk.gov.hmrc"                   %% "bootstrap-backend-play-30"        % bootstrapVersion,
    "com.github.java-json-tools"    % "json-schema-validator"             % "2.2.14",
    "org.json4s"                    %% "json4s-jackson"                   % "4.1.0-M4",
    "com.fasterxml.jackson.module"  %% "jackson-module-scala"             % "2.16.1"
  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply(): Seq[ModuleID] = new TestDependencies {
      override lazy val test = Seq(
      "uk.gov.hmrc"                   %% "bootstrap-test-play-30"     % bootstrapVersion    % scope
      )
    }.test
  }

  def apply(): Seq[ModuleID] = compile ++ Test()
}

