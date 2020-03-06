package io.github.ssledz

import cats._
import cats.data.ReaderT._
import cats.data._

object Main extends App {

  val length: Reader[String, Int] = for {
    content <- ask[Id, String]
  } yield content.length

  val mLength = local[Id, Int, String](_ + "Prefix")(length)

  val content = "12345"

  println(length.run(content))
  println(mLength.run(content))

  type Env = Map[String, Int]

  def lookup(key: String)(env: Env): Int = env.getOrElse(key, 0)

  val isCountCorrect: Reader[Env, Boolean] = for {
    env <- ask[Id, Env]
    count = lookup("count")(env)
  } yield count == env.size

  val sampleEnv = Map("count" -> 3, "1" -> 1, "2" -> 2)
  println("is count correct: " + isCountCorrect.run(sampleEnv))

}
