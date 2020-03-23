package io.github.ssledz.iabtcf

import io.github.ssledz.iabtcf.fp.Show._

object Main extends App {

  val tcString = "COvsGAAOwuA2-BxABCENAdCAAAAAAAAAAAAAB5wAQB5gAAAA.IF0EWSQgCYWgho0QUBzBAIYAfJgSCAMgSAAQIoSkFQISERBAGOiAQHAEQJAAAGBAAkACAAQAoHGBMCQABgAARiRCEQUGIDzNIBIBAggEaYUFAAAVmmkHC3ZCY702yumQ.YAAAAAAAAAAAAAAAAAA"

  val core = TCString.parse(tcString).core

  println(core.show)

}
