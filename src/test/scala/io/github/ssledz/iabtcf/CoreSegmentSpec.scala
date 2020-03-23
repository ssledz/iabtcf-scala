package io.github.ssledz.iabtcf

import io.github.ssledz.iabtcf.CoreSegmentSpec.{bracket, toTestCases}
import io.github.ssledz.iabtcf.TCString.CoreSegment
import io.github.ssledz.iabtcf.fp.Show
import io.github.ssledz.iabtcf.fp.Show._
import org.scalatest.flatspec.AnyFlatSpec

import scala.annotation.tailrec
import scala.io.Source

trait CoreSegmentSpec extends AnyFlatSpec {

  def filePath: String

  implicit def showInstance: Show[CoreSegment]

  behavior of "TCString.parse in case of valid consent string"

  def testCoreSegment(): Unit = {

    bracket(Source.fromFile(classOf[CoreSegmentSpec].getClassLoader.getResource(filePath).toURI)) { lines =>

      val tests = toTestCases(lines)

      for (test <- tests) {

        val title = test.title.stripPrefix("==").stripSuffix("==")

        it should s"return valid tc model with $title" in {

          val core = TCString.parse(test.tcfStr).core

          assert(test.parsed === core.show)

        }

      }
    }
  }

}

object CoreSegmentSpec {

  def bracket(source: Source)(f: List[String] => Unit): Unit =
    try {
      f(source.getLines.toList)
    } catch {
      case e: Throwable =>
        e.printStackTrace
        source.close()
    }


  case class SegmentTestCase(title: String, tcfStr: String, parsed: String)

  def toTestCases(in: List[String]): List[SegmentTestCase] = {

    def isTitle(l: String) = l.startsWith("==") && l.endsWith("==")

    def isComment(l: String) = l.startsWith("#")

    def isBlank(l: String) = l.trim.isEmpty

    @tailrec
    def go(in: List[String], st: List[String], acc: List[SegmentTestCase]): List[SegmentTestCase] = (in, st) match {
      case (Nil, Nil) => acc
      case (Nil, parsed :: tcfStr :: title :: Nil) => SegmentTestCase(title, tcfStr, parsed) :: acc
      case (h :: t, Nil) if isTitle(h) => go(t, List(h), acc)
      case (h :: t, _ :: Nil) if !isBlank(h) || !isComment(h) => go(t, h :: st, acc)
      case (h :: t, _ :: _ :: Nil) if !isBlank(h) && !isComment(h) => go(t, h :: st, acc)
      case (h :: t, parsed :: _ :: _) if !isBlank(h) && !isComment(h) => go(t, (parsed + "\n" + h) :: st.tail, acc)
      case (h :: t, parsed :: tcfStr :: title :: Nil) => go(t, if (isTitle(h)) List(h) else List.empty, SegmentTestCase(title, tcfStr, parsed) :: acc)
      case _ => go(in.tail, st, acc)
    }

    go(in, List.empty, List.empty)

  }

  implicit val indexedBoolSeqShowInstance: Show[IndexedSeq[Boolean]] = new Show[IndexedSeq[Boolean]] {
    def show(a: IndexedSeq[Boolean]): String = a.zipWithIndex.filter(_._1).map { case (_, i) => s"${i + 1} -> true" }.toString
  }

}
