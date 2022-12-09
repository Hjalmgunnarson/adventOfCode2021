lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "hjalmgunnarson",
      scalaVersion := "2.13.6"
    )),
    name := "adventOfCode2021"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test
