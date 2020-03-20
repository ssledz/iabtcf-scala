package io.github.ssledz

import io.github.ssledz.fp.Show._

object Main extends App {

  //Vector(129 -> true, 133 -> true, 153 -> true, 215 -> true, 224 -> true, 243 -> true)
//  val tcString = "COwgQ7JOwgQ7JExAAAENAPCAAAAAAAAAAAAAB5wBgBAgCFAEyANcAcAA8wDzgCgBigD-AQAA1wBwADzAAA.IFoEUQQgAIQwgIwQABAEAAAAOIAACAIAAAAQAIAgEAACEAAAAAgAQBAAAAAAAGBAAgAAAAAAAFAAECAAAgAAQARAEQAAAAAJAAIAAgAAAYQEAAAQmAgBC3ZAYzUw"
  val tcString = "BOOUMOpOOUPt5ABABADEAd-AAAAVl7_______9____7_9uz_Gv_r_ff_3nW0739P1A_r_Oz_qG_6xzVo4_FpQAQ"

  val core = TCString.parse(tcString).core

  println(core.show)
  println(core.version)
//  println(core.vendorConsents.hasConsent(242))
//  println(core.vendorConsents.hasConsent(243))
//  println(core.vendorLegitimateInterest.established(243))

}
