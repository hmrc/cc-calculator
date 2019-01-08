resolvers += Resolver.url("HMRC Sbt Plugin Releases", url("https://dl.bintray.com/hmrc/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

resolvers += "HMRC Releases" at "https://dl.bintray.com/hmrc/releases"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "scoverage-bintray" at "https://dl.bintray.com/sksamuel/sbt-plugins/"

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

addSbtPlugin("uk.gov.hmrc" % "sbt-auto-build" % "1.13.0")

addSbtPlugin("uk.gov.hmrc" % "sbt-git-versioning" % "1.15.0")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "0.8.5")

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.5.19")

addSbtPlugin("uk.gov.hmrc" % "sbt-distributables" % "1.2.0")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.3")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.5.1")

addSbtPlugin("uk.gov.hmrc" % "sbt-artifactory" % "0.14.0")