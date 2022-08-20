// import Mill dependency
import os.Path
import mill._
import scalalib._
import publish._
import coursier.maven.MavenRepository
import $file.`rocket-chip`.common
import $file.`rocket-chip`.`api-config-chipsalliance`.`build-rules`.mill.build
import $file.`rocket-chip`.hardfloat.build

// support BSP
object ivys {
  val sv = "2.12.8"
  val chisel3 = ivy"edu.berkeley.cs::chisel3:3.5.1"
  val chisel3Plugin = ivy"edu.berkeley.cs:::chisel3-plugin:3.5.0"
  val chiseltest = ivy"edu.berkeley.cs::chiseltest:0.5.0"
  val chiselCirct = ivy"com.sifive::chisel-circt:0.5.0"
  val scalatest = ivy"org.scalatest::scalatest:3.2.2"
  val macroParadise = ivy"org.scalamacros:::paradise:2.1.1"
  val iotesters = ivy"edu.berkeley.cs::chisel-iotester:2.5.0"
  // val rocketchip = ivy"edu.berkeley.cs::rocketchip:1.2.6"
}

trait ZirconModule extends ScalaModule with PublishModule {
  def chiselOpt: Option[PublishModule] = None
  override def scalaVersion = ivys.sv
  override def compileIvyDeps = Agg(ivys.macroParadise)
  override def scalacPluginIvyDeps = Agg(ivys.macroParadise, ivys.chisel3Plugin)
  override def scalacOptions = Seq("-Xsource:2.11")
  override def ivyDeps = (if (chiselOpt.isEmpty) Agg(ivys.chisel3) else Agg.empty[Dep]) ++ Agg(ivys.chiselCirct)
  override def moduleDeps = Seq() ++ chiselOpt

  def publishVersion = "0.0.1"

  def pomSettings = PomSettings(
    description = "Zircon",
    organization = "ysyx-kaiyuan",
    url = "",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("vanavasin", "zircon"),
    developers = Seq.empty
  )
}

object rocketchip extends `rocket-chip`.common.CommonRocketChip {
  val rcPath = os.pwd / "rocket-chip"
  override def scalaVersion = ivys.sv
  override def scalacOptions = Seq("-Xsource:2.11")
  override def millSourcePath = rcPath

  object configRocket extends `rocket-chip`.`api-config-chipsalliance`.`build-rules`.mill.build.config with PublishModule {
    override def millSourcePath = rcPath / "api-config-chipsalliance" / "design" / "craft"
    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }
    override def pomSettings = T {
      rocketchip.pomSettings()
    }
    override def publishVersion = T {
      rocketchip.publishVersion()
    }
  }

  object hardfloatRocket extends `rocket-chip`.hardfloat.build.hardfloat {
    override def millSourcePath = rcPath / "hardfloat"
    override def scalaVersion = T {
      rocketchip.scalaVersion()
    }
    def chisel3IvyDeps = if(chisel3Module.isEmpty) Agg(
      common.getVersion("chisel3")
    ) else Agg.empty[Dep]
    def chisel3PluginIvyDeps = Agg(common.getVersion("chisel3-plugin", cross=true))
  }
  def hardfloatModule = hardfloatRocket
  def configModule = configRocket

}

object huancun extends ZirconModule with SbtModule {
  override def millSourcePath: Path = os.pwd / "huancun"
  override def moduleDeps = super.moduleDeps ++ Seq(
    rocketchip
  )
}

object difftest extends ZirconModule with SbtModule {
  override def millSourcePath: Path = os.pwd / "difftest"
}

trait CommonZircon extends ZirconModule with SbtModule { m =>
  def difftestModule: PublishModule
  def rocketModule: PublishModule
  def huancunModule: PublishModule

  override def millSourcePath: Path = os.pwd
  override def ivyDeps = super.ivyDeps() ++ Seq(ivys.chiseltest)
  override def forkArgs = Seq("-Xmx8G", "-Xss256m")
  override def moduleDeps = super.moduleDeps ++ Seq(
    difftestModule,
    rocketModule,
    huancunModule
  )

  object test extends Tests with TestModule.ScalaTest {
    override def forkArgs = m.forkArgs
    override def ivyDeps = super.ivyDeps() ++ Agg(
      ivys.scalatest
    )
  }
}

object zircon extends CommonZircon {
  override def difftestModule = difftest
  override def rocketModule = rocketchip
  override def huancunModule = huancun
}
