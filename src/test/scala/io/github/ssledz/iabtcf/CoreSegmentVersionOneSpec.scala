package io.github.ssledz.iabtcf

import java.time.ZoneId

import io.github.ssledz.iabtcf.CoreSegmentSpec._
import io.github.ssledz.iabtcf.TCString.CoreSegment
import io.github.ssledz.iabtcf.TCString.CoreSegment.VendorConsents._
import io.github.ssledz.iabtcf.fp.Show
import io.github.ssledz.iabtcf.fp.Show._

class CoreSegmentVersionOneSpec extends CoreSegmentSpec {

   val filePath: String = "tcf-1.0-spec.txt"

  val showInstance: Show[CoreSegment] = new Show[CoreSegment] {
    def show(a: CoreSegment): String =
      s"""
         |version                  : ${a.version}
         |created                  : ${a.created.withZoneSameInstant(ZoneId.of("Europe/Warsaw"))}
         |updated                  : ${a.updated.withZoneSameInstant(ZoneId.of("Europe/Warsaw"))}
         |cmpId                    : ${a.cmpId}
         |cmpVersion               : ${a.cmpVersion}
         |consentScreen            : ${a.consentScreen}
         |consentLanguage          : ${a.consentLanguage}
         |vendorListVersion        : ${a.vendorListVersion}
         |purposesConsent          : ${a.purposesConsent.show}
         |vendorConsents           : ${a.vendorConsents.show}
         |""".stripMargin.trim
  }

  behavior of "parse in case of valid tcf 1.0 consent string"

  behave like testCoreSegment()

}
