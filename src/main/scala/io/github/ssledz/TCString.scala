package io.github.ssledz

import java.util.Base64

import enumeratum.values.{IntEnum, IntEnumEntry}

object TCString {

  def parse(value: String): TCModel = {
    TCModel(CoreSegment(value.split('.').head))
  }

  sealed trait TCSegment

  case class CoreSegment(value: String) extends TCSegment {

    private lazy val arr = Base64.getDecoder.decode(value)

    lazy val version: Int = Decoder[Int].decode(0, arr)

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
