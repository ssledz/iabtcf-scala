package io.github.ssledz

object Main extends App {

  val tcString = "COwaId9OwaId9LDAAAENAPCAAAIAAAAAAAAAAAAAAAAA.IFoEUQQgAIQwgIwQABAEAAAAOIAACAIAAAAQAIAgEAACEAAAAAgAQBAAAAAAAGBAAgAAAAAAAFAAECAAAgAAQARAEQAAAAAJAAIAAgAAAYQEAAAQmAgBC3ZAYzUw"

  val core = TCString.parse(tcString).core

  println(core)
  println(core.version)
  println(core.created)
  println(core.updated)
  println(core.cmpId)
  println(core.cmpVersion)
  println(core.consentScreen)
  println(core.consentLanguage)
  println(core.vendorListVersion)
  println(core.tcfPolicyVersion)
  println(core.isServiceSpecific)
  println(core.useNonStandardStacks)
  println(core.specialFeatureOptIns)
  println(core.purposesConsent)

}
