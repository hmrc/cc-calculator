import sbt._

object MicroServiceBuild extends Build with MicroService {

  val appName = "cc-calculator"

  override lazy val appDependencies: Seq[ModuleID] = AppDependencies()
}

private object AppDependencies {

  import play.core.PlayVersion
  import play.sbt.PlayImport._

  private val microServiceBootstrapVersion = "8.0.0"
  private val scalaTestVersion = "3.0.0"
  private val pegDownVersion = "1.6.0"
  private val hmrcTestVersion = "3.0.0"
  private val jsonSchemaValidator = "2.2.6"
  private val json4s = "3.3.0"
  private val mockitoVersion = "2.18.3"
  private val scalaTestPlusVersion = "2.0.0"

  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "microservice-bootstrap" % microServiceBootstrapVersion,
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

