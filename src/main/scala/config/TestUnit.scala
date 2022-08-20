package config

import freechips.rocketchip.config.{Config, Parameters}
import zircon.common._

object ZirconTestUnit {
  private def augment(tp: ZirconTileParams)(implicit p: Parameters): Parameters = p.alterPartial {
    case TileKey => tp
  }
  def getZirconParameters(configName: String, configPackage: String = "config"): Parameters = {
    val fullConfigName = configPackage + "." + configName
    val origParams: Parameters = try {
      Class.forName(fullConfigName).newInstance().asInstanceOf[Config] ++ Parameters.empty
    } catch {
      case e: java.lang.ClassNotFoundException =>
        throw new Exception(s"""Unable to find config $fullConfigName""", e)
    }
    val zirconTileParams = origParams(TileKey)
    val outParams = augment(zirconTileParams)(origParams)
    outParams
  }

}