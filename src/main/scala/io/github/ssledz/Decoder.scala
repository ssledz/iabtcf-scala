package io.github.ssledz

import java.time.{Instant, ZoneId, ZonedDateTime}

sealed trait Decoder[T] {
  def decode(offset: Int, arr: Array[Byte]): T
}

object Decoder {
  def apply[T: Decoder]: Decoder[T] = implicitly[Decoder[T]]

  implicit val boolDecoder: Decoder[Boolean] = new Decoder[Boolean] {
    def decode(offset: Int, arr: Array[Byte]): Boolean = arr.bit(offset)
  }

  implicit val int6Decoder: Decoder[Int6] = new Decoder[Int6] {
    def decode(offset: Int, arr: Array[Byte]): Int6 = Int6(arr.int(offset, 6))
  }

  implicit val int12Decoder: Decoder[Int12] = new Decoder[Int12] {
    def decode(offset: Int, arr: Array[Byte]): Int12 = Int12(arr.int(offset, 12))
  }

  implicit val char6Decoder: Decoder[Char6] = new Decoder[Char6] {
    def decode(offset: Int, arr: Array[Byte]): Char6 = Char6(('a' + arr.int(offset, 6)).toChar)
  }

  implicit val dateTimeDecoder: Decoder[ZonedDateTime] = new Decoder[ZonedDateTime] {
    def decode(offset: Int, arr: Array[Byte]): ZonedDateTime =
      ZonedDateTime.ofInstant(Instant.ofEpochSecond(arr.long(offset, 36) / 10), ZoneId.systemDefault())
  }

  private val str2Decoder: Decoder[String] = new Decoder[String] {
    def decode(offset: Int, arr: Array[Byte]): String =
      List(0, 6).map(i => Decoder[Char6].decode(offset + i, arr).value.toString).foldLeft("")(_ + _)
  }

  implicit val langDecoder: Decoder[Lang] = new Decoder[Lang] {
    def decode(offset: Int, arr: Array[Byte]): Lang = Lang(str2Decoder.decode(offset, arr).toUpperCase)
  }

  implicit val countryDecoder: Decoder[Country] = new Decoder[Country] {
    def decode(offset: Int, arr: Array[Byte]): Country = Country(str2Decoder.decode(offset, arr).toUpperCase)
  }

  case class Lang(value: String) extends AnyVal

  case class Country(value: String) extends AnyVal

  case class Int6(value: Int) extends AnyVal

  case class Char6(value: Char) extends AnyVal

  case class Int12(value: Int) extends AnyVal

  case class Long36(value: Long) extends AnyVal

  private implicit class BitSet(val array: Array[Byte]) extends AnyVal {

    def bit(offset: Int): Boolean = {
      val byteIndex = offset / 8
      if (byteIndex > array.length - 1) {
        val size = byteIndex + 1
        throw new IndexOutOfBoundsException(s"Expected consent string to contain at least $size bytes, but found only ${array.length} bytes")
      }
      val bitExact = offset % 8
      val b = array(byteIndex)
      (b & BitSet.Powers(bitExact)) != 0
    }

    def int(offset: Int, size: Int): Int = number[Int](offset, size)

    def long(offset: Int, size: Int): Long = number[Long](offset, size)

    private def number[N: Manifest](offset: Int, size: Int)(implicit N: IntNumber[N]): N = {

      if (size > N.size) {
        val clazzName = implicitly[Manifest[N]].runtimeClass.getName
        throw new IllegalArgumentException(s"can't fit bit range in $clazzName: $size")
      }
      var res: N = N.zero
      val sigMask: N = N.one
      var sigIndex: Int = size - 1

      import IntNumber._

      for (i <- 0 until size) {
        if (bit(offset + i)) res = res + (sigMask << sigIndex)
        sigIndex -= 1
      }
      res

    }

  }

  object BitSet {
    private val Powers: Array[Int] = Array(-128, 64, 32, 16, 8, 4, 2, 1)
  }

  sealed trait IntNumber[T] {

    def zero: T

    def one: T

    def size: Int

    def +(left: T, right: T): T

    def <<(left: T, right: Int): T
  }

  object IntNumber {

    def apply[N: IntNumber]: IntNumber[N] = implicitly[IntNumber[N]]

    implicit class IntNumberSyntax[N](val left: N) extends AnyVal {
      def +(right: N)(implicit N: IntNumber[N]): N = N.+(left, right)

      def <<(right: Int)(implicit N: IntNumber[N]): N = N.<<(left, right)
    }

    implicit val intInstance: IntNumber[Int] = new IntNumber[Int] {
      def zero: Int = 0

      def one: Int = 1

      def +(left: Int, right: Int): Int = left + right

      def <<(left: Int, right: Int): Int = left << right

      val size: Int = Integer.SIZE
    }

    implicit val longInstance: IntNumber[Long] = new IntNumber[Long] {
      def zero: Long = 0

      def one: Long = 1

      def +(left: Long, right: Long): Long = left + right

      def <<(left: Long, right: Int): Long = left << right

      val size: Int = java.lang.Long.SIZE
    }

  }

}