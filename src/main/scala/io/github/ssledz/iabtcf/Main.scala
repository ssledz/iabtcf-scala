package io.github.ssledz.iabtcf

import io.github.ssledz.iabtcf.fp.Show._

object Main extends App {

  val tcString = "COwBOpCOwBOpCLqAAAENAPCAAAAAAAAAAAAAFfwAQFfgUbABAUaAAA.IFoEUQQgAIQwgIwQABAEAAAAOIAACAIAAAAQAIAgEAACEAAAAAgAQBAAAAAAAGBAAgAAAAAAAFAAECAAAgAAQARAEQAAAAAJAAIAAgAAAYQEAAAQmAgBC3ZAYzUw"

  val core = TCString.parse(tcString).core

  println(core.show)

}
