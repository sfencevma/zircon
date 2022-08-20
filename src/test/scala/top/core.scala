package top

import config._
import freechips.rocketchip.config.Config

class ZirconConfig extends Config (
  new WithNMegaZircon(0)
)