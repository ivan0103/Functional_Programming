package dataset

import dataset.Part5A._
import org.scalatest.FunSuite

class Part5ATest extends FunSuite {

	val patrons = scala.io.Source.fromResource("library-first6000.csv").getLines().toList.drop(1).map(line => line.split(","))

	test("Q25 - highest number of checkouts") {
		assertResult((3205,"60 to 64 years","phone")) {
			highestCheckouts(patrons)
		}
	}

	test("Q26 - no age provided") {
		assertResult(2) {
			noAge(patrons)
		}
	}

	test("Q27 - missing email as a map") {
		assertResult(Map("ADULT" -> 1)) {
			missing_email(patrons)
		}
	}
}