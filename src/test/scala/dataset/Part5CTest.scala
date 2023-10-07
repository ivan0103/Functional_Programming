package dataset

import Part5C._
import dataset.util.Commit.Commit
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization._
import org.scalatest.FunSuite

import scala.io.Source

class Part5CTest extends FunSuite {

    implicit val formats: AnyRef with Formats = Serialization.formats(NoTypeHints)
    val source: List[Commit] = Source.fromResource("1000_commits.json").getLines().map(Serialization.read[Commit]).toList

    test("Q31 - most productive part of day example") {
        assertResult(("afternoon", 992)) {
            mostProductivePart(source)
        }
    }
}