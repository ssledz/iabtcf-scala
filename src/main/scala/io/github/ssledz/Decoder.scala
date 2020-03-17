package io.github.ssledz

import java.time.LocalDateTime

sealed trait Decoder[T] {
  def decode(offset: Int, arr: Array[Byte]): T
}

object Decoder {
  def apply[T: Decoder]: Decoder[T] = implicitly[Decoder[T]]

  implicit val intDecoder: Decoder[Int] = new Decoder[Int] {
    def decode(offset: Int, arr: Array[Byte]): Int = arr.int(offset, 6)
  }

  implicit val dateTimeDecoder: Decoder[LocalDateTime] = new Decoder[LocalDateTime] {
    def decode(offset: Int, arr: Array[Byte]): LocalDateTime = ???
  }

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