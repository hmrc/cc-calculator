import sbt._

object MicroServiceBuild extends Build with MicroService {

  val appName = "cc-calculator"

  override lazy val appDependencies: Seq[ModuleID] = AppDependencies()
}

private object AppDependencies {

  import play.sbt.PlayImport._
  import play.core.PlayVersion

  private val microServiceBootstrapVersion = "5.13.0"
  private val playConfigVersion = "4.3.0"
  private val playAuthorisationVersion = "4.3.0"
  private val logbackJsonLoggerVersion = "3.1.0"
  private val playUrlBindersVersion = "2.1.0"
  private val playSchedulingVersion = "4.1.0"
  private val playHealthVersion = "2.1.0"
  private val scalaTestVersion = "2.2.6"
  private val pegDownVersion = "1.6.0"
  private val hmrcTestVersion = "2.3.0"
  private val jsonSchemaValidator = "2.2.6"
  private val json4s = "3.2.11"
  private val mockitoVersion = "1.9.0"
  private val scalaTestPlusVersion = "1.5.1"

  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "microservice-bootstrap" % microServiceBootstrapVersion,
    "uk.gov.hmrc" %% "play-url-binders" % playUrlBindersVersion,
    "uk.gov.hmrc" %% "play-config" % playConfigVersion,
    "uk.gov.hmrc" %% "logback-json-logger" % logbackJsonLoggerVersion,
    "uk.gov.hmrc" %% "play-authorisation" % playAuthorisationVersion,
    "uk.gov.hmrc" %% "play-scheduling" % playSchedulingVersion,
    "uk.gov.hmrc" %% "play-health" % playHealthVersion,
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
        "org.scalatestplus.play" %% "scalatestplus-play" % scalaTestPlusVersion % scope,
        "uk.gov.hmrc" %% "hmrctest" % hmrcTestVersion % scope,
        "org.scalatest" %% "scalatest" % scalaTestVersion % scope,
        "org.pegdown" % "pegdown" % pegDownVersion % scope,
        "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
        "org.mockito" % "mockito-core" % mockitoVersion % scope
      )
    }.test
  }

  def apply() = compile ++ Test()
}

