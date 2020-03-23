package io.github.ssledz.iabtcf

import java.time.{Instant, ZoneId, ZonedDateTime}

import io.github.ssledz.iabtcf.Decoder.DecodedResult
import io.github.ssledz.iabtcf.fp.Show

import scala.annotation.tailrec

trait Decoder[A] {

  self =>

  def decode(offset: Int, arr: Array[Byte]): DecodedResult[A]

  def ap[B](fab: Decoder[A => B]): Decoder[B] =
    for {
      f <- fab
      a <- self
    } yield f(a)

  def map2[B, C](fb: Decoder[B])(f: (A, B) => C): Decoder[C] = for {
    a <- self
    b <- fb
  } yield f(a, b)

  def map[B](f: A => B): Decoder[B] = flatMap(a => Decoder.pure(f(a)))

  def flatMap[B](f: A => Decoder[B]): Decoder[B] = new Decoder[B] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[B] = {
      val DecodedResult(newOffset, a) = self.decode(offset, arr)
      f(a).decode(newOffset, arr)
    }
  }

}

object Decoder {

  def apply[A: Decoder]: Decoder[A] = implicitly[Decoder[A]]

  def pure[A](value: A): Decoder[A] = new Decoder[A] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[A] = DecodedResult(offset, value)
  }

  def tupled[A, B](fa: Decoder[A], fb: Decoder[B]): Decoder[(A, B)] = fa.map2(fb)(_ -> _)

  def sequence[A](xs: IndexedSeq[Decoder[A]]): Decoder[IndexedSeq[A]] = {

    @tailrec
    def go(xs: IndexedSeq[Decoder[A]], acc: Decoder[IndexedSeq[A]]): Decoder[IndexedSeq[A]] = xs.headOption match {
      case Some(ha) =>
        val newAcc = for {
          a <- ha
          as <- acc
        } yield a +: as
        go(xs.tail, newAcc)
      case None => acc
    }

    go(xs, Decoder.pure(Vector.empty))
  }

  def sequenceDecoder[A: Decoder](size: Int): Decoder[IndexedSeq[A]] = sequence(Vector.fill(size)(Decoder[A]))

  implicit val boolDecoder: Decoder[Boolean] = new Decoder[Boolean] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[Boolean] = DecodedResult(offset + 1, arr.bit(offset))
  }

  implicit val int6Decoder: Decoder[Int6] = new Decoder[Int6] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[Int6] = DecodedResult(offset + 6, Int6(arr.int(offset, 6)))
  }

  implicit val int2Decoder: Decoder[Int2] = new Decoder[Int2] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[Int2] = DecodedResult(offset + 2, Int2(arr.int(offset, 2)))
  }

  implicit val int12Decoder: Decoder[Int12] = new Decoder[Int12] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[Int12] = DecodedResult(offset + 12, Int12(arr.int(offset, 12)))
  }

  implicit val int16Decoder: Decoder[Int16] = new Decoder[Int16] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[Int16] = DecodedResult(offset + 16, Int16(arr.int(offset, 16)))
  }

  implicit val char6Decoder: Decoder[Char6] = new Decoder[Char6] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[Char6] = DecodedResult(offset + 6, Char6(('a' + arr.int(offset, 6)).toChar))
  }

  implicit val dateTimeDecoder: Decoder[ZonedDateTime] = new Decoder[ZonedDateTime] {
    def decode(offset: Int, arr: Array[Byte]): DecodedResult[ZonedDateTime] =
      DecodedResult(offset + 36, ZonedDateTime.ofInstant(Instant.ofEpochSecond(arr.long(offset, 36) / 10), ZoneId.systemDefault()))
  }

  private val str2Decoder: Decoder[String] = for {
    a <- Decoder[Char6]
    b <- Decoder[Char6]
  } yield a.value.toString + b.value.toString

  implicit val langDecoder: Decoder[Lang] = str2Decoder.map(str => Lang(str.toUpperCase))

  implicit val countryDecoder: Decoder[Country] = str2Decoder.map(str => Country(str.toUpperCase))

  implicit val intRangeDecoder: Decoder[IntRange] = new Decoder[IntRange] {

    implicit val boolInt16Decoder: Decoder[(Boolean, Int16)] = Decoder.tupled(Decoder[Boolean], Decoder[Int16])

    def decode(offset: Int, arr: Array[Byte]): DecodedResult[IntRange] = {

      @tailrec
      def go(offset: Int, numEntries: Int, ranges: List[Range] = List.empty, elems: Set[Int] = Set.empty): DecodedResult[IntRange] =
        if (numEntries == 0) {
          DecodedResult(offset, DefaultIntRange(elems, ranges))
        } else {
          val DecodedResult(offsetB, (isRange, startIdx)) = Decoder[(Boolean, Int16)].decode(offset, arr)
          if (isRange) {
            val DecodedResult(offsetC, endIdx) = Decoder[Int16].decode(offsetB, arr)
            go(offsetC, numEntries - 1, (startIdx.value to endIdx.value) :: ranges, elems)
          } else {
            go(offsetB, numEntries - 1, ranges, elems + startIdx.value)
          }
        }

      val DecodedResult(offsetA, numEntries) = Decoder[Int12].decode(offset, arr)

      go(offsetA, numEntries.value)
    }
  }

  private def bitFieldDecoder(size: Int): Decoder[IntRange] =
    Decoder.sequenceDecoder[Boolean](size).map { xs =>
      import collection.mutable

      val res = mutable.Set.empty[Int]
      var i = 0
      while(i < xs.size) {
        if(xs(i)) {
          res += i + 1
        }
        i = i + 1
      }
      DefaultIntRange(res.toSet)
    }

  implicit val legacyIntSetDecoder: Decoder[LegacyIntSet] = for {
    maxId <- Decoder[Int16]
    isRange <- Decoder[Boolean]
    decoder = if (isRange) {
      Decoder.tupled(Decoder[Boolean].map(Option.apply), intRangeDecoder)
    } else {
      Decoder.tupled(Decoder.pure(None: Option[Boolean]), bitFieldDecoder(maxId.value))
    }
    decodedRes <- decoder
    (defaultValue, intRange) = decodedRes
  } yield defaultValue match {
    case Some(value) => LegacyIntSet(IntSetImpl(maxId.value, intRange.withDefaultValue(value)))
    case None => LegacyIntSet(IntSetImpl(maxId.value, intRange))
  }

  implicit val intSetDecoder: Decoder[IntSet] = for {
    maxId <- Decoder[Int16]
    isRange <- Decoder[Boolean]
    decoder = if (isRange) intRangeDecoder else bitFieldDecoder(maxId.value)
    intRange <- decoder
  } yield IntSetImpl(maxId.value, intRange)

  /**
   * @param offset - current offset after decoding value
   */
  case class DecodedResult[A](offset: Int, value: A)

  sealed trait IntSet {
    def contains(key: Int): Boolean
  }

  case class LegacyIntSet(underlying: IntSet) extends AnyVal

  object IntSet {
    implicit val intSetShowInstance: Show[IntSet] = new Show[IntSet] {
      def show(a: IntSet): String = a match {
        case s@IntSetImpl(max, _) => (1 to max).filter(s.contains).map(i => s"$i -> true").toString
        case _ => "???"
      }
    }
  }

  private case class IntSetImpl(max: Int, range: IntRange) extends IntSet {
    def contains(key: Int): Boolean = if (key > max) false else range.contains(key)
  }

  sealed trait IntRange {
    def contains(key: Int): Boolean

    def toSeq: Seq[Int]

    protected[iabtcf] def withDefaultValue(value: Boolean): IntRange
  }

  object IntRange {
    val empty: IntRange = DefaultIntRange()
    implicit val intRangeShowInstance: Show[IntRange] = new Show[IntRange] {
      def show(a: IntRange): String = a match {
        case DefaultIntRange(elems, ranges) => (ranges.toSet.flatMap((r: Range) => r.toSet) ++ elems).toList.sorted.toString
        case _ => "???"
      }
    }
  }

  private case class IntRangeWithDefaultValue(elems: Set[Int] = Set.empty, ranges: List[Range] = List.empty, defaultValue: Boolean = false) extends IntRange {
    def contains(key: Int): Boolean = if (elems.contains(key) || ranges.exists(_.contains(key))) !defaultValue else defaultValue

    def toSeq: Seq[Int] = ranges.flatMap(_.toSet) ++ elems

    protected[iabtcf] def withDefaultValue(value: Boolean): IntRange = this
  }

  private case class DefaultIntRange(elems: Set[Int] = Set.empty, ranges: List[Range] = List.empty) extends IntRange {
    def contains(key: Int): Boolean = elems.contains(key) || ranges.exists(_.contains(key))

    def toSeq: Seq[Int] = ranges.flatMap(_.toSet) ++ elems

    protected[iabtcf] def withDefaultValue(value: Boolean): IntRange = IntRangeWithDefaultValue(elems, ranges, value)
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

      var i = 0
      while(i < size) {
        if (bit(offset + i)) res = res + (sigMask << sigIndex)
        sigIndex -= 1
        i = i + 1
      }

      res

    }

  }

  object BitSet {
    private val Powers: Array[Int] = Array(128, 64, 32, 16, 8, 4, 2, 1)
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