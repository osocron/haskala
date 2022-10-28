val zioVersion = "2.0.2"
val catsVersion = "2.6.1"
val catsEffectVersion = "3.1.1"
val requestsVersion = "0.7.0"

lazy val root = project
  .in(file("."))
  .settings(
    inThisBuild(
      List(
        name := "scala-side",
        organization := "com.geophy",
        version := "0.0.1",
        scalaVersion := "3.2.0",
        scalacOptions ++= Seq(
          "-Xcheck-macros"
        )
      )
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "dev.zio" %% "zio" % zioVersion,
      "com.lihaoyi" %% "requests" % requestsVersion
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
