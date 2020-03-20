package io.github.ssledz

import io.github.ssledz.TCString.CoreSegment._
import io.github.ssledz.fp.Show._

object Main extends App {

  //Vector(129 -> true, 133 -> true, 153 -> true, 215 -> true, 224 -> true, 243 -> true)
  val tcString = "COwgQ7JOwgQ7JExAAAENAPCAAAAAAAAAAAAAB5wBgBAgCFAEyANcAcAA8wDzgCgBigD-AQAA1wBwADzAAA.IFoEUQQgAIQwgIwQABAEAAAAOIAACAIAAAAQAIAgEAACEAAAAAgAQBAAAAAAAGBAAgAAAAAAAFAAECAAAgAAQARAEQAAAAAJAAIAAgAAAYQEAAAQmAgBC3ZAYzUw"

  val core = TCString.parse(tcString).core

  println(core.show)
//  println(core.vendorConsents.hasConsent(242))
//  println(core.vendorConsents.hasConsent(243))
//  println(core.vendorLegitimateInterest.established(243))

}
