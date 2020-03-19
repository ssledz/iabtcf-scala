package io.github.ssledz

import io.github.ssledz.TCString.CoreSegment._
import io.github.ssledz.fp.Show._

object Main extends App {

  val tcString = "COwgQ7JOwgQ7JExAAAENAPCAAAAAAAAAAAAAB5wBgBAgCFAEyANcAcAA8wDzgCgBigD-AQAA1wBwADzAAA.IFoEUQQgAIQwgIwQABAEAAAAOIAACAIAAAAQAIAgEAACEAAAAAgAQBAAAAAAAGBAAgAAAAAAAFAAECAAAgAAQARAEQAAAAAJAAIAAgAAAYQEAAAQmAgBC3ZAYzUw"

  val core = TCString.parse(tcString).core

  println(core.show)
  println(core.vendorConsents.hasConsent(242))
  println(core.vendorConsents.hasConsent(243))

}
