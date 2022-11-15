name := "SudokuSolver"

version := "0.1"

scalaVersion := "3.2.0"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.14",
  "org.scalatest" %% "scalatest" % "3.2.14" % "test"
)

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.8.0",
)
