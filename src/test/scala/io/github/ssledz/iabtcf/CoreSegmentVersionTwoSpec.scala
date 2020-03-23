package io.github.ssledz.iabtcf

import io.github.ssledz.iabtcf.CoreSegmentSpec._
import io.github.ssledz.iabtcf.TCString.CoreSegment.VendorConsents._
import io.github.ssledz.iabtcf.TCString.{CoreSegment, CoreSegmentVersionTwo}
import io.github.ssledz.iabtcf.fp.Show
import io.github.ssledz.iabtcf.fp.Show._

class CoreSegmentVersionTwoSpec extends CoreSegmentSpec {

  val filePath: String = "tcf-2.0-spec.txt"

  val showInstance: Show[CoreSegment] = new Show[CoreSegment] {
    def show(a: CoreSegment): String = a match {
      case aa: CoreSegmentVersionTwo =>
        s"""
           |version                  : ${aa.version}
           |created                  : ${aa.created}
           |updated                  : ${aa.updated}
           |cmpId                    : ${aa.cmpId}
           |cmpVersion               : ${aa.cmpVersion}
           |consentScreen            : ${aa.consentScreen}
           |consentLanguage          : ${aa.consentLanguage}
           |vendorListVersion        : ${aa.vendorListVersion}
           |tcfPolicyVersion         : ${aa.tcfPolicyVersion}
           |isServiceSpecific        : ${aa.isServiceSpecific}
           |useNonStandardStacks     : ${aa.useNonStandardStacks}
           |specialFeatureOptIns     : ${aa.specialFeatureOptIns.show}
           |purposesConsent          : ${aa.purposesConsent.show}
           |purposesLITransparency   : ${aa.purposesLITransparency.show}
           |purposeOneTreatment      : ${aa.purposeOneTreatment}
           |publisherCountryCode     : ${aa.publisherCountryCode}
           |vendorConsents           : ${aa.vendorConsents.show}
           |vendorLegitimateInterest : ${aa.vendorLegitimateInterest.show}
           |publisherRestrictions    : ${aa.publisherRestrictions.show}
           |""".stripMargin.trim
      case _ => fail("expected tcf 2.0 core segment")
    }
  }

  val tcString = "COwkr9uOwkr9uNPAAAENAdCAADwAAAAAAAAAAHgCSAAAAAA"

  val c = TCString.parse(tcString).core

  println(c.show(showInstance))

  behave like testCoreSegment

  behavior of "parse in case of valid tcf 2.0 consent string"

}
