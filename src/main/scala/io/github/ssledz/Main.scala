package io.github.ssledz

object Main extends App {

  val tcString = "COwaId9OwaId9LDAAAENAPCAAAIAAAAAAAAAAAAAAAAA.IFoEUQQgAIQwgIwQABAEAAAAOIAACAIAAAAQAIAgEAACEAAAAAgAQBAAAAAAAGBAAgAAAAAAAFAAECAAAgAAQARAEQAAAAAJAAIAAgAAAYQEAAAQmAgBC3ZAYzUw"

  val core = TCString.parse(tcString).core

  println(core)
  println(core.version)

}
