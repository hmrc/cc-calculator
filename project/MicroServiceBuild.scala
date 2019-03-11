import sbt._

object MicroServiceBuild extends Build with MicroService {

  val appName = "cc-calculator"

  override lazy val appDependencies: Seq[ModuleID] = AppDependencies()
}

private object AppDependencies {

  import play.core.PlayVersion
  import play.sbt.PlayImport._


  val compile: Seq[ModuleID] = Seq(
    ws,
    "uk.gov.hmrc" %% "bootstrap-play-26" % "0.37.0",
    "com.github.fge" % "json-schema-validator" % "2.2.6",
    "org.json4s" %% "json4s-jackson" % "3.3.0",
    "uk.gov.hmrc" %% "govuk-template" % "5.30.0-play-26",
    "uk.gov.hmrc" %% "play-ui" % "7.33.0-play-26",
    "com.typesafe.play" %% "play-json" % "2.6.13",
    "com.typesafe.play" %% "play-json-joda" % "2.6.13"
  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply(): Seq[ModuleID] = new TestDependencies {
      override lazy val test = Seq(
        "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.0" % scope,
        "uk.gov.hmrc" %% "hmrctest" % "3.6.0-play-26" % scope,
        "org.scalatest" %% "scalatest" % "3.0.6" % scope,
        "org.pegdown" % "pegdown" % "1.6.0" % scope,
        "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
        "org.mockito" % "mockito-core" % "2.25.0" % scope
      )
    }.test
  }

  def apply(): Seq[ModuleID] = compile ++ Test()
}

