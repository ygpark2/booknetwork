name := """booknetwork"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "3.7.4"

resolvers ++= Seq(
  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
  "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  jdbc,
  ws,
  "org.playframework" %% "play-slick" % "6.2.0",
  "org.playframework.silhouette" %% "play-silhouette" % "10.0.4",
  "org.playframework.silhouette" %% "play-silhouette-password-bcrypt" % "10.0.4",
  "org.playframework.silhouette" %% "play-silhouette-persistence" % "10.0.4",
  "com.h2database" % "h2" % "2.4.240",
  "org.xerial" % "sqlite-jdbc" % "3.49.1.0"
)
