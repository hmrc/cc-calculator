import sbt._

object MicroServiceBuild extends Build with MicroService {

  import scala.util.Properties.envOrElse

  val appName = "cc-calculator"

  override lazy val appDependencies: Seq[ModuleID] = AppDependencies()
}

private object AppDependencies {

  import play.PlayImport._
  import play.core.PlayVersion

  private val microServiceBootstrapVersion = "4.2.1"
  private val playConfigVersion = "2.0.1"
  private val playAuthorisationVersion = "3.1.0"
  private val playJsonLoggerVersion = "2.1.1"
  private val playUrlBindersVersion = "1.0.0"
  private val playSchedulingVersion = "1.1.0"
  private val playHealthVersion = "1.1.0"
  private val scalaTestVersion = "2.2.2"
  private val pegDownVersion = "1.4.2"
  private val hmrcTestVersion = "1.6.0"
  private val jsonSchemaValidator = "2.2.6"
  private val json4s = "3.2.11"
  private val httpVerbs = "3.3.0"

  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "microservice-bootstrap" % microServiceBootstrapVersion,
    "uk.gov.hmrc" %% "play-url-binders" % playUrlBindersVersion,
    "uk.gov.hmrc" %% "play-config" % playConfigVersion,
    "uk.gov.hmrc" %% "play-json-logger" % playJsonLoggerVersion,
    "uk.gov.hmrc" %% "play-authorisation" % playAuthorisationVersion,
    "uk.gov.hmrc" %% "play-scheduling" % playSchedulingVersion,
    "uk.gov.hmrc" %% "play-health" % playHealthVersion,
    "uk.gov.hmrc" %% "http-verbs" % httpVerbs,
    "com.github.fge" % "json-schema-validator" % jsonSchemaValidator,
    "org.json4s" %% "json4s-jackson" % json4s
  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply() = new TestDependencies {
      override lazy val test = Seq(
        "org.scalatestplus" %% "play" % "1.2.0" % scope,
        "uk.gov.hmrc" %% "hmrctest" % hmrcTestVersion % scope,
        "org.scalatest" %% "scalatest" % scalaTestVersion % scope,
        "org.pegdown" % "pegdown" % pegDownVersion % scope,
        "com.typesafe.play" %% "play-test" % PlayVersion.current % scope
      )
    }.test
  }

  def apply() = compile ++ Test()
}

