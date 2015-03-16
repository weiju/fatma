name         := "fatma"

version      := "1.0"

organization := "org.dmpp"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-unchecked", "-deprecation")

seq(sbtassembly.Plugin.assemblySettings: _*)
