package io.github.ssledz

import java.time.ZonedDateTime
import java.util.Base64

import enumeratum.values.{IntEnum, IntEnumEntry}
import io.github.ssledz.Decoder.{Char6, Int12, Int6}

object TCString {

  def parse(value: String): TCModel = {
    TCModel(CoreSegment(value.split('.').head))
  }

  sealed trait TCSegment

  case class CoreSegment(value: String) extends TCSegment {

    private lazy val arr = Base64.getDecoder.decode(value)

    lazy val version: Int = Decoder[Int6].decode(0, arr).value
    lazy val created: ZonedDateTime = Decoder[ZonedDateTime].decode(6, arr)
    lazy val updated: ZonedDateTime = Decoder[ZonedDateTime].decode(42, arr)
    lazy val cmpId: Int = Decoder[Int12].decode(78, arr).value
    lazy val cmpVersion: Int = Decoder[Int12].decode(90, arr).value
    lazy val consentScreen: Int = Decoder[Int6].decode(102, arr).value
    lazy val consentLanguage: String = List(
      Decoder[Char6].decode(108, arr).value.toString,
      Decoder[Char6].decode(114, arr).value.toString
    ).foldLeft("")(_ + _).toUpperCase
    lazy val vendorListVersion: Int = Decoder[Int12].decode(120, arr).value
    lazy val tcfPolicyVersion: Int = Decoder[Int6].decode(132, arr).value
    lazy val isServiceSpecific: Boolean = Decoder[Boolean].decode(138, arr)
    lazy val useNonStandardStacks: Boolean = Decoder[Boolean].decode(139, arr)
    lazy val specialFeatureOptIns: IndexedSeq[Boolean] = (0 until 12).toVector.map(i => Decoder[Boolean].decode(140 + i, arr))
    lazy val purposesConsent: IndexedSeq[Boolean] = (0 until 24).toVector.map(i => Decoder[Boolean].decode(152 + i, arr))

  }

  case class TCModel(core: CoreSegment, segments: Map[TCSegmentType, TCSegment] = Map.empty) {
  }

  sealed abstract class TCSegmentType private(val value: Int) extends IntEnumEntry

  object TCSegmentType extends IntEnum[TCSegmentType] {
    val values: IndexedSeq[TCSegmentType] = findValues

    case object Core extends TCSegmentType(0)

    case object VendorsDisclosed extends TCSegmentType(1)

    case object VendorsAllowed extends TCSegmentType(2)

    case object PublisherTC extends TCSegmentType(3)

  }

}
