name := "advanced_scala"

version := "1.0"

scalaVersion := "2.10.4"


resolvers += "Akka Repository" at "http://repo.akka.io/releases/"

resolvers ++= Seq(
            // other resolvers here
            // if you want to use snapshot builds (currently 0.8-SNAPSHOT), use this.
            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
            "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"
)
