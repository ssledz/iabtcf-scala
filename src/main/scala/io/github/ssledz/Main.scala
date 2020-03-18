package io.github.ssledz

import io.github.ssledz.TCString.CoreSegment._
import io.github.ssledz.fp.Show._

object Main extends App {

  val tcString = "COwaId9OwaId9LDAAAENAPCAAAIAAAAAAAAAAAAAAAAA.IFoEUQQgAIQwgIwQABAEAAAAOIAACAIAAAAQAIAgEAACEAAAAAgAQBAAAAAAAGBAAgAAAAAAAFAAECAAAgAAQARAEQAAAAAJAAIAAgAAAYQEAAAQmAgBC3ZAYzUw"

  val core = TCString.parse(tcString).core

  println(core.show)

}
