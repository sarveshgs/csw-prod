import sbt.Keys._
import sbt._

object AutoMultiJvm extends AutoPlugin {
  import com.typesafe.sbt.SbtMultiJvm
  import SbtMultiJvm.MultiJvmKeys._
  import sbtassembly.Plugin.AssemblyKeys._
  import sbtassembly.Plugin.MergeStrategy

  override def projectSettings: Seq[Setting[_]] = SbtMultiJvm.multiJvmSettings ++ Seq(
    test := {
      (test in Test).value
      (test in MultiJvm).value
    },
    multiNodeHosts in MultiJvm := multiNodeHostNames,
    mergeStrategy in assembly in MultiJvm := {
      case "application.conf" => MergeStrategy.concat
      case x =>
        val oldStrategy = (mergeStrategy in assembly in MultiJvm).value
        oldStrategy(x)
    }
  )

  def multiNodeHostNames: Seq[String] = sys.env.get("multiNodeHosts") match {
    case Some(str) ⇒ str.split(",").toSeq
    case None      ⇒ Seq.empty
  }

  override def projectConfigurations: Seq[Configuration] = List(MultiJvm)
}
