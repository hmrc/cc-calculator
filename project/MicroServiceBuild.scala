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
    "uk.gov.hmrc" %% "bootstrap-play-25" % "4.9.0",
    "com.github.fge" % "json-schema-validator" % "2.2.6",
    "org.json4s" %% "json4s-jackson" % "3.3.0",
    "uk.gov.hmrc" %% "govuk-template" % "5.30.0-play-25",
    "uk.gov.hmrc" %% "play-ui" % "7.33.0-play-25"
  )

  trait TestDependencies {
    lazy val scope: String = "test"
    lazy val test: Seq[ModuleID] = ???
  }

  object Test {
    def apply(): Seq[ModuleID] = new TestDependencies {
      override lazy val test = Seq(
        "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.0" % scope,
        "uk.gov.hmrc" %% "hmrctest" % "3.4.0-play-25" % scope,
        "org.scalatest" %% "scalatest" % "3.0.0" % scope,
        "org.pegdown" % "pegdown" % "1.6.0" % scope,
        "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
        "org.mockito" % "mockito-core" % "2.18.3" % scope
      )
    }.test
  }

  def apply(): Seq[ModuleID] = compile ++ Test()
}

