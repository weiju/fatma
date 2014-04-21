name         := "fatma"

version      := "1.0"

organization := "org.dmpp"

scalaVersion := "2.11.0"

scalacOptions ++= Seq("-unchecked", "-deprecation")

seq(sbtassembly.Plugin.assemblySettings: _*)
