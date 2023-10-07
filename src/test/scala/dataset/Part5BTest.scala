package dataset

import Part5B._
import dataset.util.XMLDatafile.Badge
import org.scalatest.FunSuite

import scala.io.Source

class Part5BTest extends FunSuite {

  val exampleSource: List[Badge] = Part5B.source

  test("Q28 - reading in file and get 11th element") {
    assertResult(Badge(12,24,"Autobiographer","2012-03-06T18:53:16.300",3,false)) {
      exampleSource(11)
    }
  }

  test("Q29 - easiest attainable badge") {
    assertResult(("Autobiographer",1066)) {
      easiestAttainableBadge(exampleSource)
    }
  }

  test("Q30 - yearOverview") {
    assertResult(((2013,32),(2012,6168)) ) {
      yearOverview(exampleSource)
    }
  }
}
