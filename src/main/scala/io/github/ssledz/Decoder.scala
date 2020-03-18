package io.github.ssledz

import java.time.{Instant, ZoneId, ZonedDateTime}
import java.util.concurrent.atomic.AtomicReference

import io.github.ssledz.Decoder.MemoizedDecoder
import io.github.ssledz.fp.Show

import scala.annotation.tailrec

sealed trait Decoder[T] {
  def decode(offset: Int, arr: Array[Byte]): T

  def memo: Decoder[T] = MemoizedDecoder(this)
}

object Decoder {
  def apply[T: Decoder]: Decoder[T] = implicitly[Decoder[T]]

  private case class MemoizedDecoder[T](underlying: Decoder[T], ref: AtomicReference[T] = new AtomicReference[T]) extends Decoder[T] {
    def decode(offset: Int, arr: Array[Byte]): T = ref.updateAndGet((t: T) => if (t == null) decode(offset, arr) else t)
  }

  implicit val boolDecoder: Decoder[Boolean] = new Decoder[Boolean] {
    def decode(offset: Int, arr: Array[Byte]): Boolean = arr.bit(offset)
  }

  implicit val int6Decoder: Decoder[Int6] = new Decoder[Int6] {
    def decode(offset: Int, arr: Array[Byte]): Int6 = Int6(arr.int(offset, 6))
  }

  implicit val int12Decoder: Decoder[Int12] = new Decoder[Int12] {
    def decode(offset: Int, arr: Array[Byte]): Int12 = Int12(arr.int(offset, 12))
  }

  implicit val int16Decoder: Decoder[Int16] = new Decoder[Int16] {
    def decode(offset: Int, arr: Array[Byte]): Int16 = Int16(arr.int(offset, 16))
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

  implicit val intSetDecoderDecoder: Decoder[IntSetDecoder] = new Decoder[IntSetDecoder] {

    def rangeDec(maxId: Int): Decoder[(Int, IntSet)] = new Decoder[(Int, IntSet)] {

      def decode(offset: Int, arr: Array[Byte]): (Int, IntSet) = {

        val numEntries = Decoder[Int12].decode(offset, arr).value

//        println("numEntries: " + numEntries)

        @tailrec
        def go(offset: Int, numEntries: Int, size: Int, ranges: List[Range] = List.empty, elems: Set[Int] = Set.empty): (Int, IntSet) =
          if (numEntries == 0) {
            size -> IntSetImpl(maxId, elems, ranges)
          } else {
            val isRange = Decoder[Boolean].decode(offset, arr)
            val startIdx = Decoder[Int16].decode(offset + 1, arr).value
//            println("isRange: " + isRange)
//            println("startIdx: " + startIdx)
            if (isRange) {
              val endIdx = Decoder[Int16].decode(offset + 17, arr).value
              go(offset + 33, numEntries - 1, size + 33, (startIdx to endIdx) :: ranges , elems)
            } else {
              go(offset + 17, numEntries - 1, size + 17, ranges, elems + startIdx)
            }
          }

        go(offset + 12, Decoder[Int12].decode(offset, arr).value, 12)

      }
    }

    def bitFieldDec(size: Int): Decoder[IntSet] = new Decoder[IntSet] {
      def decode(offset: Int, arr: Array[Byte]): IntSet = IntSetImpl(0)
    }

    def decode(offset: Int, arr: Array[Byte]): IntSetDecoder = {

      val maxId = Decoder[Int16].decode(offset, arr).value
      val isRange = Decoder[Boolean].decode(offset + 16, arr)

//      println("maxId: " + maxId)
//      println("isRange: " + isRange)

      if (isRange) {

        val (size, intSet) = rangeDec(maxId).decode(offset + 17, arr)

        val decoder: Decoder[IntSet] = new Decoder[IntSet] {
          def decode(offset: Int, arr: Array[Byte]): IntSet = intSet
        }
        IntSetDecoder(offset, size + 17, arr, decoder)
      } else {
        IntSetDecoder(offset, maxId + 17, arr, bitFieldDec(maxId))
      }
    }
  }

  case class IntSetDecoder(offset: Int, size: Int, arr: Array[Byte], underlying: Decoder[IntSet]) {
    def decode: IntSet = underlying.decode(offset, arr)
  }

  sealed trait IntSet {
    def contains(key: Int): Boolean
  }

  object IntSet {
    implicit val intSetShowInstance: Show[IntSet] = {
      case s@IntSetImpl(max, _, _) => (1 to max).filter(s.contains).map(i => s"$i -> true").toString
      case _ => "???"
    }
  }

  private case class IntSetImpl(max: Int, elems: Set[Int] = Set.empty, ranges: List[Range] = List.empty) extends IntSet {
    def contains(key: Int): Boolean = elems.contains(key) || ranges.exists(_.contains(key))
  }

  case class Lang(value: String) extends AnyVal

  case class Country(value: String) extends AnyVal

  case class Int6(value: Int) extends AnyVal

  case class Char6(value: Char) extends AnyVal

  case class Int12(value: Int) extends AnyVal

  case class Int16(value: Int) extends AnyVal

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