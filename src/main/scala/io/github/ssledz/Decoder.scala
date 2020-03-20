package io.github.ssledz

import java.time.{Instant, ZoneId, ZonedDateTime}

import io.github.ssledz.Decoder.DecodedResult
import io.github.ssledz.fp.Show

import scala.annotation.tailrec

trait Decoder[A] {

  self =>

  def decode(offset: Int, arr: Array[Byte]): DecodedResult[A]

  def ap[B](fab: Decoder[A => B]): Decoder[B] =
    for {
      f <- fab
      a <- self
    } yield f(a)

  def map2[B, C](fb: Decoder[B])(f: (A, B) => C): Decoder[C] = {
    val fabc: Decoder[A => B => C] = Decoder.pure(f.curried)
    val fbc: Decoder[B => C] = ap(fabc)
    fb.ap(fbc)
  }

  def flatMap[B](f: A => Decoder[B]): Decoder[B] = new Decoder[B] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[B] = {
      val DecodedResult(size, newOffset, a) = self.decode(offset, arr)
      val res = f(a).decode(newOffset, arr)
      res.copy(size = res.size + size)
    }
  }

  def map[B](f: A => B): Decoder[B] = new Decoder[B] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[B] = {
      val res = self.decode(offset, arr)
      res.copy(value = f(res.value))
    }
  }

}

object Decoder {

  def apply[A: Decoder]: Decoder[A] = implicitly[Decoder[A]]

  def pure[A](value: A): Decoder[A] = new Decoder[A] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[A] = DecodedResult(0, offset, value)
  }

  def tupled[A, B](fa: Decoder[A], fb: Decoder[B]): Decoder[(A, B)] = fa.map2(fb)(_ -> _)

  def sequence[A](xs: IndexedSeq[Decoder[A]]): Decoder[IndexedSeq[A]] = {

    def go(xs: IndexedSeq[Decoder[A]], acc: Decoder[IndexedSeq[A]]): Decoder[IndexedSeq[A]] = xs.headOption match {
      case Some(ha) =>
        val newAcc = for {
        a <- ha
        as <- acc
      } yield as :+ a
        go(xs.tail, newAcc)
      case None => acc
    }

    go(xs, Decoder.pure(Vector.empty))
  }

  def sequenceDecoder[A: Decoder](size: Int): Decoder[IndexedSeq[A]] = sequence(Vector.fill(size)(Decoder[A]))

  implicit val boolDecoder: Decoder[Boolean] = new Decoder[Boolean] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[Boolean] = DecodedResult(1, offset + 1, arr.bit(offset))
  }

  implicit val int6Decoder: Decoder[Int6] = new Decoder[Int6] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[Int6] = DecodedResult(6, offset + 6, Int6(arr.int(offset, 6)))
  }

  implicit val int2Decoder: Decoder[Int2] = new Decoder[Int2] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[Int2] = DecodedResult(2, offset + 2, Int2(arr.int(offset, 2)))
  }

  implicit val int12Decoder: Decoder[Int12] = new Decoder[Int12] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[Int12] = DecodedResult(12, offset + 12, Int12(arr.int(offset, 12)))
  }

  implicit val int16Decoder: Decoder[Int16] = new Decoder[Int16] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[Int16] = DecodedResult(16, offset + 16, Int16(arr.int(offset, 16)))
  }

  implicit val char6Decoder: Decoder[Char6] = new Decoder[Char6] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[Char6] = DecodedResult(6, offset + 6, Char6(('a' + arr.int(offset, 6)).toChar))
  }

  implicit val dateTimeDecoder: Decoder[ZonedDateTime] = new Decoder[ZonedDateTime] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[ZonedDateTime] =
      DecodedResult(36, offset + 36, ZonedDateTime.ofInstant(Instant.ofEpochSecond(arr.long(offset, 36) / 10), ZoneId.systemDefault()))
  }

  private val str2Decoder: Decoder[String] = for {
    a <- Decoder[Char6]
    b <- Decoder[Char6]
  } yield a.value.toString + b.value.toString

  //  private val str2Decoder: Decoder[String] = new Decoder[String] {
  //    def decode(offset: Int, arr: Array[Byte]): String =
  //      List(0, 6).map(i => Decoder[Char6].decode(offset + i, arr).value.toString).foldLeft("")(_ + _)
  //  }

  implicit val langDecoder: Decoder[Lang] = str2Decoder.map(str => Lang(str.toUpperCase))

  //  implicit val langDecoder: Decoder[Lang] = new Decoder[Lang] {
  //    def decode(offset: Int, arr: Array[Byte]): Lang = Lang(str2Decoder.decode(offset, arr).toUpperCase)
  //  }

  implicit val countryDecoder: Decoder[Country] = str2Decoder.map(str => Country(str.toUpperCase))

  //  implicit val countryDecoder: Decoder[Country] = new Decoder[Country] {
  //    def decode(offset: Int, arr: Array[Byte]): Country = Country(str2Decoder.decode(offset, arr).toUpperCase)
  //  }

  implicit val intRangeDecoder: Decoder[IntRange] = new Decoder[IntRange] {

    def decode(offset: Int, arr: Array[Byte]): DecodedResult[IntRange] = {

      val DecodedResult(_, _, numEntries) = Decoder[Int12].decode(offset, arr)

      @tailrec
      def go(offset: Int, numEntries: Int, size: Int, ranges: List[Range] = List.empty, elems: Set[Int] = Set.empty): DecodedResult[IntRange] =
        if (numEntries == 0) {
          DecodedResult(size, offset, IntRangeImpl(elems, ranges))
        } else {

          val DecodedResult(_, newOffset, (isRange, startIdx)) = Decoder.tupled(Decoder[Boolean], Decoder[Int16]).decode(offset, arr)

          //          val isRange = Decoder[Boolean].decode(offset, arr)
          //          val startIdx = Decoder[Int16].decode(offset + 1, arr).value
          if (isRange) {
            //            val endIdx = Decoder[Int16].decode(offset + 17, arr).value
            val endIdx = Decoder[Int16].decode(newOffset, arr).value
            go(newOffset + 16, numEntries - 1, size + 33, (startIdx.value to endIdx.value) :: ranges, elems)
          } else {
            go(newOffset, numEntries - 1, size + 17, ranges, elems + startIdx.value)
          }
        }

      go(offset + 12, numEntries.value, 12)
    }
  }

  implicit val intSetDecoderDecoder: Decoder[IntSetDecoder] = new Decoder[IntSetDecoder] {

    def bitFieldDec(size: Int): Decoder[IntSet] = new Decoder[IntSet] {
      def decode(offset: Int, arr: Array[Byte]): DecodedResult[IntSet] = ???
    }

    def decode(offset: Int, arr: Array[Byte]): DecodedResult[IntSetDecoder] = {

      val DecodedResult(_, newOffset, (maxId, isRange)) = Decoder.tupled(Decoder[Int16], Decoder[Boolean]).decode(offset, arr)

      //      val maxId = Decoder[Int16].decode(offset, arr).value
      //      val isRange = Decoder[Boolean].decode(offset + 16, arr)

      if (isRange) {

        val DecodedResult(size, _, intRange) = intRangeDecoder.decode(newOffset, arr)

        val decoder: Decoder[IntSet] = Decoder.pure(IntSetImpl(maxId.value, intRange))

        DecodedResult(0, 0, IntSetDecoder(offset, size + 17, arr, decoder))
      } else {
        DecodedResult(0, 0, IntSetDecoder(offset, maxId.value + 17, arr, bitFieldDec(maxId.value)))
      }
    }
  }

  /**
   * @size - how much bits were consumed to decode the value
   */
  case class DecodedResult[A](size: Int, offset: Int, value: A)

  case class IntSetDecoder(offset: Int, size: Int, arr: Array[Byte], underlying: Decoder[IntSet]) {
    def decode: IntSet = underlying.decode(offset, arr).value
  }

  sealed trait IntSet {
    def contains(key: Int): Boolean
  }

  object IntSet {
    implicit val intSetShowInstance: Show[IntSet] = {
      case s@IntSetImpl(max, _) => (1 to max).filter(s.contains).map(i => s"$i -> true").toString
      case _ => "???"
    }
  }

  private case class IntSetImpl(max: Int, range: IntRange) extends IntSet {
    def contains(key: Int): Boolean = range.contains(key)
  }

  sealed trait IntRange {
    def contains(key: Int): Boolean

    def toSeq: Seq[Int]
  }

  object IntRange {
    val empty: IntRange = IntRangeImpl()
    implicit val intRangeShowInstance: Show[IntRange] = {
      case IntRangeImpl(elems, ranges) => (ranges.toSet.flatMap((r: Range) => r.toSet) ++ elems).toList.sorted.toString
      case _ => "???"
    }
  }

  private case class IntRangeImpl(elems: Set[Int] = Set.empty, ranges: List[Range] = List.empty) extends IntRange {
    def contains(key: Int): Boolean = elems.contains(key) || ranges.exists(_.contains(key))

    def toSeq: Seq[Int] = ranges.map(_.toSet).flatten ++ elems
  }

  case class Lang(value: String) extends AnyVal

  case class Country(value: String) extends AnyVal

  case class Int6(value: Int) extends AnyVal

  case class Int2(value: Int) extends AnyVal

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