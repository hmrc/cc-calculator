import scoverage.ScoverageKeys
import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion

lazy val appName = "cc-calculator"

lazy val appDependencies: Seq[ModuleID] = ???
lazy val plugins: Seq[Plugins]          = Seq(SbtDistributablesPlugin)
lazy val playSettings: Seq[Setting[?]]  = Seq.empty

ThisBuild / majorVersion := 1
ThisBuild / scalaVersion := "3.3.7"

lazy val scoverageSettings =
  Seq(
    ScoverageKeys.coverageExcludedPackages := "<empty>;Reverse.*;.*.Routes.*;routes_routing.*;uk.gov.hmrc;config.*;prod.*",
    ScoverageKeys.coverageMinimumStmtTotal := 95,
    ScoverageKeys.coverageFailOnMinimum    := true,
    ScoverageKeys.coverageHighlighting     := true,
    Test / parallelExecution               := false
  )

lazy val microservice = Project(appName, file("."))
  .enablePlugins(Seq(play.sbt.PlayScala) ++ plugins: _*)
  .disablePlugins(JUnitXmlReportPlugin)
  .settings(playSettings ++ scoverageSettings: _*)
  .settings(PlayKeys.playDefaultPort := 9372)
  .settings(
    libraryDependencies ++= AppDependencies.all,
    dependencyOverrides += "commons-codec" % "commons-codec" % "1.12",
    retrieveManaged                       := true,
    routesGenerator                       := InjectedRoutesGenerator,
    resolvers += Resolver.jcenterRepo
  )
  .settings(
    scalacOptions ++= Seq(
      // Silence unused warnings on Play `routes` files
      "-Wconf:msg=.*unused import.*&src=.*routes.*:s",
      "-Wconf:msg=.*unused private member.*&src=.*routes.*:s"
    )
  )
