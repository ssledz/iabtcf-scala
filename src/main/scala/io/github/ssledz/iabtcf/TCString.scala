package io.github.ssledz.iabtcf

import java.time.ZonedDateTime
import java.util.Base64

import enumeratum.EnumEntry
import enumeratum.values.{IntEnum, IntEnumEntry}
import io.github.ssledz.iabtcf.Decoder._
import io.github.ssledz.iabtcf.TCString.CoreSegment.VendorConsents
import io.github.ssledz.iabtcf.TCString.CoreSegmentVersionTwo.PublisherRestrictions.PurposeRestriction
import io.github.ssledz.iabtcf.TCString.CoreSegmentVersionTwo._
import io.github.ssledz.iabtcf.fp.Show
import io.github.ssledz.iabtcf.fp.Show._

import scala.annotation.tailrec

object TCString {

  def parse(value: String): TCModel = {
    val segments = value.split('.')
    val coreSegment = segments.head
    val arr = Base64.getUrlDecoder.decode(coreSegment)
    val version = Decoder[Int6].decode(0, arr).value.value
    if (version == 1) {
      TCModel(CoreSegmentVersionOne(version, arr))
    } else {
      TCModel(CoreSegmentVersionTwo(version, arr))
    }
  }

  sealed trait TCSegment

  sealed trait CoreSegment extends TCSegment {

    protected def arr: Array[Byte]

    val version: Int
    lazy val created: ZonedDateTime = Decoder[ZonedDateTime].decode(6, arr).value
    lazy val updated: ZonedDateTime = Decoder[ZonedDateTime].decode(42, arr).value
    lazy val cmpId: Int = Decoder[Int12].decode(78, arr).value.value
    lazy val cmpVersion: Int = Decoder[Int12].decode(90, arr).value.value
    lazy val consentScreen: Int = Decoder[Int6].decode(102, arr).value.value
    lazy val consentLanguage: Lang = Decoder[Lang].decode(108, arr).value
    lazy val vendorListVersion: Int = Decoder[Int12].decode(120, arr).value.value

    def purposesConsent: IndexedSeq[Boolean]

    def vendorConsents: VendorConsents
  }

  object CoreSegment {

    implicit val coreShowInstance: Show[CoreSegment] = new Show[CoreSegment] {
      def show(a: CoreSegment): String = a match {
        case aa: CoreSegmentVersionOne => aa.show
        case aa: CoreSegmentVersionTwo => aa.show
      }
    }

    class VendorConsents(private val underlying: IntSet) extends AnyVal {
      def hasConsent(vendorId: Int): Boolean = underlying.contains(vendorId)
    }

    object VendorConsents {
      implicit val vendorConsentsShowInstance: Show[VendorConsents] = new Show[VendorConsents] {
        def show(a: VendorConsents): String = a.underlying.show
      }
    }

  }

  case class CoreSegmentVersionOne(version: Int, protected val arr: Array[Byte]) extends CoreSegment {

    lazy val purposesConsent: IndexedSeq[Boolean] = Decoder.sequenceDecoder[Boolean](24).decode(132, arr).value

    lazy val vendorConsents: VendorConsents = new VendorConsents(Decoder[LegacyIntSet].decode(156, arr).value.underlying)
  }

  private implicit val indexedBoolSeqShowInstance: Show[IndexedSeq[Boolean]] = new Show[IndexedSeq[Boolean]] {
    def show(a: IndexedSeq[Boolean]): String = a.zipWithIndex.filter(_._1).map { case (_, i) => s"${i + 1} -> true" }.toString
  }

  object CoreSegmentVersionOne {
    implicit val coreV1ShowInstance: Show[CoreSegmentVersionOne] = new Show[CoreSegmentVersionOne] {
      def show(a: CoreSegmentVersionOne): String =
        s"""
           |version                  : ${a.version}
           |created                  : ${a.created}
           |updated                  : ${a.updated}
           |cmpId                    : ${a.cmpId}
           |cmpVersion               : ${a.cmpVersion}
           |consentScreen            : ${a.consentScreen}
           |consentLanguage          : ${a.consentLanguage}
           |vendorListVersion        : ${a.vendorListVersion}
           |purposesConsent          : ${a.purposesConsent.show}
           |vendorConsents           : ${a.vendorConsents.show}
           |""".stripMargin
    }
  }

  case class CoreSegmentVersionTwo(version: Int, protected val arr: Array[Byte]) extends CoreSegment {

    lazy val tcfPolicyVersion: Int = Decoder[Int6].decode(132, arr).value.value
    lazy val isServiceSpecific: Boolean = Decoder[Boolean].decode(138, arr).value
    lazy val useNonStandardStacks: Boolean = Decoder[Boolean].decode(139, arr).value
    lazy val specialFeatureOptIns: IndexedSeq[Boolean] = Decoder.sequenceDecoder[Boolean](12).decode(140, arr).value
    lazy val purposesConsent: IndexedSeq[Boolean] = Decoder.sequenceDecoder[Boolean](24).decode(152, arr).value
    lazy val purposesLITransparency: IndexedSeq[Boolean] = Decoder.sequenceDecoder[Boolean](24).decode(176, arr).value
    lazy val purposeOneTreatment: Boolean = Decoder[Boolean].decode(200, arr).value
    lazy val publisherCountryCode: Country = Decoder[Country].decode(201, arr).value

    private lazy val vendorConsentsDecodedResult = Decoder[IntSet].decode(213, arr)

    lazy val vendorConsents: VendorConsents = new VendorConsents(vendorConsentsDecodedResult.value)

    private lazy val vendorLegitimateInterestDecodedResult = Decoder[IntSet].decode(vendorConsentsDecodedResult.offset, arr)

    lazy val vendorLegitimateInterest: VendorLegitimateInterest = new VendorLegitimateInterest(vendorLegitimateInterestDecodedResult.value)

    lazy val publisherRestrictions: Option[PublisherRestrictions] =
      Decoder[Option[PublisherRestrictions]].decode(vendorLegitimateInterestDecodedResult.offset, arr).value

  }

  object CoreSegmentVersionTwo {

    implicit val coreV2ShowInstance: Show[CoreSegmentVersionTwo] = new Show[CoreSegmentVersionTwo] {
      def show(a: CoreSegmentVersionTwo): String =
        s"""
           |version                  : ${a.version}
           |created                  : ${a.created}
           |updated                  : ${a.updated}
           |cmpId                    : ${a.cmpId}
           |cmpVersion               : ${a.cmpVersion}
           |consentScreen            : ${a.consentScreen}
           |consentLanguage          : ${a.consentLanguage}
           |vendorListVersion        : ${a.vendorListVersion}
           |tcfPolicyVersion         : ${a.tcfPolicyVersion}
           |isServiceSpecific        : ${a.isServiceSpecific}
           |useNonStandardStacks     : ${a.useNonStandardStacks}
           |specialFeatureOptIns     : ${a.specialFeatureOptIns.show}
           |purposesConsent          : ${a.purposesConsent.show}
           |purposesLITransparency   : ${a.purposesLITransparency.show}
           |purposeOneTreatment      : ${a.purposeOneTreatment}
           |publisherCountryCode     : ${a.publisherCountryCode}
           |vendorConsents           : ${a.vendorConsents.show}
           |vendorLegitimateInterest : ${a.vendorLegitimateInterest.show}
           |publisherRestrictions    : ${a.publisherRestrictions.show}
           |""".stripMargin
    }

    case class PublisherRestrictions(private val xs: List[(PurposeRestriction, IntRange)]) {

      lazy val allVendors: Set[Int] = xs.flatMap { case (_, vs) => vs.toSeq }.toSet

      lazy val allRestrictions: Set[PurposeRestriction] = xs.map(_._1).toSet

      def restrictions(vendorId: Int): Set[PurposeRestriction] = xs.foldLeft(Set.empty[PurposeRestriction]) { case (acc, (pr, range)) =>
        if (range.contains(vendorId)) {
          acc + pr
        } else {
          acc
        }
      }

      def vendors(restriction: PurposeRestriction): Set[Int] = xs.foldLeft(Set.empty[Int]) { case (acc, (pr, range)) =>
        if (restriction == pr) {
          acc ++ range.toSeq
        } else {
          acc
        }
      }

    }

    object PublisherRestrictions {

      implicit val publisherRestrictionsDecoder: Decoder[Option[PublisherRestrictions]] = new Decoder[Option[PublisherRestrictions]] {

        implicit val vendorPurposeRestrictionDecoder: Decoder[(PurposeRestriction, IntRange)] = for {
          restriction <- Decoder[PurposeRestriction]
          vendorRange <- Decoder[IntRange]
        } yield restriction -> vendorRange

        def decode(offset: Int, arr: Array[Byte]): DecodedResult[Option[PublisherRestrictions]] = {

          val DecodedResult(newOffset, numOfRestrictions) = Decoder[Int12].decode(offset, arr)

          if (numOfRestrictions.value == 0) {
            DecodedResult(newOffset, None)
          } else {
            @tailrec
            def go(offset: Int, cnt: Int, acc: List[(PurposeRestriction, IntRange)] = List.empty): DecodedResult[List[(PurposeRestriction, IntRange)]] = {
              if (cnt == 0) {
                DecodedResult(offset, acc)
              } else {
                val DecodedResult(newOffset, restriction) = Decoder[(PurposeRestriction, IntRange)].decode(offset, arr)
                go(newOffset, cnt - 1, restriction :: acc)
              }
            }

            val DecodedResult(offset, restrictions) = go(newOffset, numOfRestrictions.value)
            DecodedResult(offset, Some(PublisherRestrictions(restrictions)))
          }
        }
      }

      implicit val publisherRestrictionsShowInstance: Show[PublisherRestrictions] = new Show[PublisherRestrictions] {
        def show(prs: PublisherRestrictions): String = prs.xs.sortBy { case (r, _) => r.purposeId }
          .map { case (restriction, vendorRange) => s"${restriction.show} -> ${vendorRange.show}" }.toString
      }

      case class PurposeRestriction(purposeId: Int, restrictionType: RestrictionType)

      object PurposeRestriction {

        implicit val purposeRestrictionDecoder: Decoder[PurposeRestriction] =
          for {
            purposeId <- Decoder[Int6]
            restrictionType <- Decoder[RestrictionType]
          } yield PurposeRestriction(purposeId.value, restrictionType)

        implicit val purposeRestrictionShowInstance: Show[PurposeRestriction] = new Show[PurposeRestriction] {
          override def show(pr: PurposeRestriction): String = s"${pr.purposeId}::${pr.restrictionType.entryName}"
        }

      }

      sealed abstract class RestrictionType private(val value: Int) extends IntEnumEntry with EnumEntry

      object RestrictionType extends IntEnum[RestrictionType] {

        implicit val restrictionTypeDecoder: Decoder[RestrictionType] = Decoder[Int2].map(id => RestrictionType.withValue(id.value))

        val values = findValues

        case object NotAllowed extends RestrictionType(0)

        case object RequireConsent extends RestrictionType(1)

        case object RequireLegitimateInterest extends RestrictionType(2)

      }

    }

    case class VendorLegitimateInterest(private val underlying: IntSet) extends AnyVal {
      def established(vendorId: Int): Boolean = underlying.contains(vendorId)
    }

    object VendorLegitimateInterest {
      implicit val vendorLegitimateInterestShowInstance: Show[VendorLegitimateInterest] = new Show[VendorLegitimateInterest] {
        def show(a: VendorLegitimateInterest): String = a.underlying.show
      }
    }

  }

  case class TCModel(core: CoreSegment, segments: Map[TCSegmentType, TCSegment] = Map.empty)

  sealed abstract class TCSegmentType private(val value: Int) extends IntEnumEntry with EnumEntry

  object TCSegmentType extends IntEnum[TCSegmentType] {
    val values = findValues

    case object Core extends TCSegmentType(0)

    case object VendorsDisclosed extends TCSegmentType(1)

    case object VendorsAllowed extends TCSegmentType(2)

    case object PublisherTC extends TCSegmentType(3)

  }

}
