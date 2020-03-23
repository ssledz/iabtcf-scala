package io.github.ssledz.iabtcf

import java.time.ZonedDateTime
import java.util.TimeZone

import io.github.ssledz.iabtcf.Decoder.{Country, Lang}
import io.github.ssledz.iabtcf.TCString.{CoreSegment, CoreSegmentVersionTwo}
import io.github.ssledz.iabtcf.fp.Show._
import org.scalatest.flatspec.AnyFlatSpec

class TCStringSpec extends AnyFlatSpec {

  behavior of "parse in case of valid tcf 1.0 consent string"

  it should "return valid core segment" in {

    val tcString = "BOOUMOpOOUPt5ABABADEAd-AAAAVl7_______9____7_9uz_Gv_r_ff_3nW0739P1A_r_Oz_qG_6xzVo4_FpQAQ"

    val core = TCString.parse(tcString).core

    assert(core.version === 1)
    assert(core.created === ZonedDateTime.parse("2018-05-25T15:45:52+02:00[Europe/Warsaw]").withZoneSameInstant(TimeZone.getDefault.toZoneId))
    assert(core.updated === ZonedDateTime.parse("2018-05-25T16:09:41+02:00[Europe/Warsaw]").withZoneSameInstant(TimeZone.getDefault.toZoneId))
    assert(core.cmpId === 1)
    assert(core.cmpVersion === 1)
    assert(core.consentScreen === 0)
    assert(core.consentLanguage === Lang("DE"))
    assert(core.vendorListVersion === 29)
    assert(core.purposesConsent === Vector(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, true))
    val vendorHasConsent = Vector(1, 13, 15, 18, 20, 21, 23, 27, 28, 29, 30, 31, 32, 36, 37, 38, 42, 44, 45, 47, 49, 51, 52, 55, 56, 57, 61, 62, 64, 66, 67, 68, 69, 70, 71, 72, 73, 74, 76, 77, 82, 84, 86, 87, 88, 89, 90, 91, 92, 93, 94, 97, 98, 100, 101, 102, 105, 106, 107, 108, 109, 110, 111, 112, 114, 116, 117, 118, 119, 120, 121, 122, 129, 131, 133, 134, 135, 136, 137, 138, 141, 143, 144, 145, 146, 147, 148, 149, 151, 152, 153, 154, 156, 157, 158, 161, 163, 164, 166, 167, 169, 171, 172, 173, 176, 177, 178, 179, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 195, 196, 197, 198, 199, 201, 202, 203, 204, 205, 206, 207, 208, 210, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 224, 226, 227, 231, 232, 233, 234, 235, 236, 237, 238, 241, 242, 244, 245, 246, 248, 249, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 342, 343, 344, 345)
    TCStringSpec.assertVendorHasConsent(core, vendorHasConsent)

  }

  behavior of "parse in case of valid tcf 2.0 consent string"

  it should "return valid core segment" in {

    val tcString = "COwkr9uOwkr9uNPAAAENAdCAADwAAAAAAAAAAHgCSAAAAAA"

    val c = TCString.parse(tcString).core

    assert(c.isInstanceOf[CoreSegmentVersionTwo])

    println(c.show)

    val core = c.asInstanceOf[CoreSegmentVersionTwo]

    assert(core.version === 2)
    assert(core.created === ZonedDateTime.parse("2020-03-20T20:01:56+01:00[Europe/Warsaw]").withZoneSameInstant(TimeZone.getDefault.toZoneId))
    assert(core.updated === ZonedDateTime.parse("2020-03-20T20:01:56+01:00[Europe/Warsaw]").withZoneSameInstant(TimeZone.getDefault.toZoneId))
    assert(core.cmpId === 847)
    assert(core.cmpVersion === 0)
    assert(core.consentScreen === 0)
    assert(core.consentLanguage === Lang("EN"))
    assert(core.vendorListVersion === 29)
    assert(core.tcfPolicyVersion === 2)
    assert(core.isServiceSpecific === false)
    assert(core.useNonStandardStacks === false)
    assert(core.specialFeatureOptIns === Vector(false, false, false, false, false, false, false, false, false, false, false, false))
    assert(core.purposesConsent === Vector(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, true, true, true, true, false, false))
    assert(core.purposesLITransparency === Vector(false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false))
    assert(core.purposeOneTreatment === false)
    assert(core.publisherCountryCode === Country("AA"))
    TCStringSpec.assertVendorHasConsent(core, Vector(1, 4, 7))

  }

}

object TCStringSpec {

  def assertVendorHasConsent(core: CoreSegment, vendorHasConsent: Vector[Int]): Unit = {
    for (id <- 1 to vendorHasConsent.max) {
      if (vendorHasConsent.contains(id)) {
        assert(core.vendorConsents.hasConsent(id), s"because expected that vendor: '$id' has user consent")
      } else {
        assert(!core.vendorConsents.hasConsent(id), s"because expected that vendor: '$id' has no user consent")
      }
    }
  }

}
