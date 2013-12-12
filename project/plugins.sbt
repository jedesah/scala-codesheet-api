addSbtPlugin("de.johoop" % "jacoco4sbt" % "2.1.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-git" % "0.6.2")

resolvers += Resolver.url(
  "bintray-sbt-plugin-releases",
    url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(
        Resolver.ivyStylePatterns)

addSbtPlugin("me.lessis" % "bintray-sbt" % "0.1.1")