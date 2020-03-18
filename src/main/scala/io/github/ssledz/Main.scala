package io.github.ssledz

import io.github.ssledz.TCString.CoreSegment._
import io.github.ssledz.fp.Show._

object Main extends App {

//  val tcString = "COwdaEaOwdaEaIpAAAENAPCAAAAAAAAAAAAAB5wAQB5gAAAA.IFoEUQQgAIQwgIwQABAEAAAAOIAACAIAAAAQAIAgEAACEAAAAAgAQBAAAAAAAGBAAgAAAAAAAFAAECAAAgAAQARAEQAAAAAJAAIAAgAAAYQEAAAQmAgBC3ZAYzUw"
  val tcString = "COwdw-LOwdw-LK_AAAENAPCAAAAAAAAAAAAADpwDgAGAASAAuACUALAAYoA_gECANcAcAA8wDUAG4AN-AdMAAAAA.IFoEUQQgAIQwgIwQABAEAAAAOIAACAIAAAAQAIAgEAACEAAAAAgAQBAAAAAAAGBAAgAAAAAAAFAAECAAAgAAQARAEQAAAAAJAAIAAgAAAYQEAAAQmAgBC3ZAYzUw"

  val core = TCString.parse(tcString).core

  println(core.show)
  println(core.vendorConsents.hasConsent(242))
  println(core.vendorConsents.hasConsent(243))

}
