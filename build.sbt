name := """booknetwork"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.13.18"

resolvers ++= Seq(
  "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
  "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)

libraryDependencies ++= Seq(
  jdbc,
  ws,
  "com.typesafe.play" %% "play-slick" % "5.1.0",
  "com.github.mohiva" %% "play-silhouette" % "7.0.0",
  "com.github.mohiva" %% "play-silhouette-password-bcrypt" % "7.0.0",
  "com.github.mohiva" %% "play-silhouette-persistence" % "7.0.0",
  "org.slf4j" % "slf4j-nop" % "2.0.16",
  "com.h2database" % "h2" % "2.4.240",
  "org.xerial" % "sqlite-jdbc" % "3.47.1.0"
)
