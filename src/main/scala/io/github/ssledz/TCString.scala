package io.github.ssledz

import java.time.ZonedDateTime
import java.util.Base64

import enumeratum.EnumEntry.Camelcase
import enumeratum.values.{IntEnum, IntEnumEntry}
import io.github.ssledz.Decoder.{Country, DecodedResult, Int12, Int2, Int6, IntRange, IntSet, IntSetDecoder, Lang}
import io.github.ssledz.TCString.CoreSegment.PublisherRestrictions.PurposeRestriction
import io.github.ssledz.TCString.CoreSegment._
import io.github.ssledz.fp.Show
import io.github.ssledz.fp.Show._

import scala.annotation.tailrec

object TCString {

  def parse(value: String): TCModel = {
    TCModel(CoreSegment(value.split('.').head))
  }

  sealed trait TCSegment

  case class CoreSegment(value: String) extends TCSegment {

    private lazy val arr = Base64.getUrlDecoder.decode(value)

    lazy val version: Int = Decoder[Int6].decode(0, arr).value.value
    lazy val created: ZonedDateTime = Decoder[ZonedDateTime].decode(6, arr).value
    lazy val updated: ZonedDateTime = Decoder[ZonedDateTime].decode(42, arr).value
    lazy val cmpId: Int = Decoder[Int12].decode(78, arr).value.value
    lazy val cmpVersion: Int = Decoder[Int12].decode(90, arr).value.value
    lazy val consentScreen: Int = Decoder[Int6].decode(102, arr).value.value
    lazy val consentLanguage: Lang = Decoder[Lang].decode(108, arr).value
    lazy val vendorListVersion: Int = Decoder[Int12].decode(120, arr).value.value
    lazy val tcfPolicyVersion: Int = Decoder[Int6].decode(132, arr).value.value
    lazy val isServiceSpecific: Boolean = Decoder[Boolean].decode(138, arr).value
    lazy val useNonStandardStacks: Boolean = Decoder[Boolean].decode(139, arr).value
    lazy val specialFeatureOptIns: IndexedSeq[Boolean] = (0 until 12).toVector.map(i => Decoder[Boolean].decode(140 + i, arr).value)
    lazy val purposesConsent: IndexedSeq[Boolean] = (0 until 24).toVector.map(i => Decoder[Boolean].decode(152 + i, arr).value)
    lazy val purposesLITransparency: IndexedSeq[Boolean] = (0 until 24).toVector.map(i => Decoder[Boolean].decode(176 + i, arr).value)
    lazy val purposeOneTreatment: Boolean = Decoder[Boolean].decode(200, arr).value
    lazy val publisherCC: Country = Decoder[Country].decode(201, arr).value
//
//    private lazy val vendorConsentsDecoder = Decoder[IntSetDecoder].decode(213, arr)
//
//    lazy val vendorConsents: VendorConsents = new VendorConsents(vendorConsentsDecoder.decode)
//
//    private lazy val vendorLegitimateInterestDecoder = Decoder[IntSetDecoder].decode(213 + vendorConsentsDecoder.size, arr)
//
//    lazy val vendorLegitimateInterest: VendorLegitimateInterest = new VendorLegitimateInterest(vendorLegitimateInterestDecoder.decode)
//
//    lazy val publisherRestrictions: Option[PublisherRestrictions] =
//      Decoder[Option[PublisherRestrictions]].decode(213 + vendorConsentsDecoder.size + vendorLegitimateInterestDecoder.size, arr)

  }

  object CoreSegment {

    implicit val coreShowInstance: Show[CoreSegment] = (a: CoreSegment) =>
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
             |specialFeatureOptIns     : ${a.specialFeatureOptIns}
             |purposesConsent          : ${a.purposesConsent}
             |purposesLITransparency   : ${a.purposesLITransparency}
             |purposeOneTreatment      : ${a.purposeOneTreatment}
             |publisherCountryCode     : ${a.publisherCC}
             |""".stripMargin
//      s"""
//         |version                  : ${a.version}
//         |created                  : ${a.created}
//         |updated                  : ${a.updated}
//         |cmpId                    : ${a.cmpId}
//         |cmpVersion               : ${a.cmpVersion}
//         |consentScreen            : ${a.consentScreen}
//         |consentLanguage          : ${a.consentLanguage}
//         |vendorListVersion        : ${a.vendorListVersion}
//         |tcfPolicyVersion         : ${a.tcfPolicyVersion}
//         |isServiceSpecific        : ${a.isServiceSpecific}
//         |useNonStandardStacks     : ${a.useNonStandardStacks}
//         |specialFeatureOptIns     : ${a.specialFeatureOptIns}
//         |purposesConsent          : ${a.purposesConsent}
//         |purposesLITransparency   : ${a.purposesLITransparency}
//         |purposeOneTreatment      : ${a.purposeOneTreatment}
//         |publisherCountryCode     : ${a.publisherCC}
//         |vendorConsents           : ${a.vendorConsents.show}
//         |vendorLegitimateInterest : ${a.vendorLegitimateInterest.show}
//         |publisherRestrictions    : ${a.publisherRestrictions.show}
//         |""".stripMargin

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

//      implicit val publisherRestrictionsDecoder: Decoder[Option[PublisherRestrictions]] = new Decoder[Option[PublisherRestrictions]] {
//
//        def decode(offset: Int, arr: Array[Byte]): Option[PublisherRestrictions] = {
//
//          val numOfRestrictions = Decoder[Int12].decode(offset, arr).value
//
//          if (numOfRestrictions == 0) {
//            None
//          } else {
//            @tailrec
//            def go(offset: Int, cnt: Int, acc: List[(PurposeRestriction, IntRange)] = List.empty): List[(PurposeRestriction, IntRange)] = {
//              if (cnt == 0) {
//                acc
//              } else {
//                val purposeId = Decoder[Int6].decode(offset, arr).value
//                val DecodedResult(rtSize, restrictionType) = Decoder[DecodedResult[RestrictionType]].decode(offset + 6, arr)
//                val DecodedResult(vendorSize, vendorRange) = Decoder[DecodedResult[IntRange]].decode(offset + 6 + rtSize, arr)
//                val restriction = PurposeRestriction(purposeId, restrictionType) -> vendorRange
//                go(offset + 6 + rtSize + vendorSize, cnt - 1, restriction :: acc)
//              }
//            }
//
//            val restrictions = go(offset + 12, numOfRestrictions)
//            Some(PublisherRestrictions(restrictions))
//          }
//        }
//      }

      implicit val publisherRestrictionsShowInstance: Show[PublisherRestrictions] = (prs: PublisherRestrictions) =>
        prs.allVendors.map(id => s"$id -> ${prs.restrictions(id).map(_.show)}").toString

      case class PurposeRestriction(purposeId: Int, restrictionType: RestrictionType)

      object PurposeRestriction {
        implicit val purposeRestrictionShowInstance: Show[PurposeRestriction] = pr => s"${pr.purposeId}::${pr.restrictionType.entryName}"
      }

      sealed abstract class RestrictionType private(val value: Int) extends IntEnumEntry with Camelcase

      object RestrictionType extends IntEnum[RestrictionType] {

//        implicit val restrictionTypeDecoder: Decoder[DecodedResult[RestrictionType]] = new Decoder[DecodedResult[RestrictionType]] {
//          def decode(offset: Int, arr: Array[Byte]): DecodedResult[RestrictionType] =
//            DecodedResult(2, RestrictionType.withValue(Decoder[Int2].decode(offset, arr).value))
//        }

        val values: IndexedSeq[RestrictionType] = findValues

        case object NotAllowed extends RestrictionType(0)

        case object RequireConsent extends RestrictionType(1)

        case object RequireLegitimateInterest extends RestrictionType(2)

      }

    }

    case class VendorLegitimateInterest(private val underlying: IntSet) extends AnyVal {
      def established(vendorId: Int): Boolean = underlying.contains(vendorId)
    }

    import IntSet._

    object VendorLegitimateInterest {
      implicit val vendorLegitimateInterestShowInstance: Show[VendorLegitimateInterest] = _.underlying.show
    }

    class VendorConsents(private val underlying: IntSet) extends AnyVal {
      def hasConsent(vendorId: Int): Boolean = underlying.contains(vendorId)
    }

    object VendorConsents {
      implicit val vendorConsentsShowInstance: Show[VendorConsents] = _.underlying.show
    }

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
