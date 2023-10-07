package basics

import basics.Part3._
import org.scalatest.FunSuite

class Part3Test extends FunSuite {

	test("Q10 - twiceAPI") {
		assertResult( List(1,1,2,2,3,3)) {
			twiceAPI(List[Int](1,2,3))
		}
	}

	test("Q11 - drunkWordsAPI") {
		assertResult( List("spoo", "rab")) {
			drunkWordsAPI(List("bar", "oops"))
		}
	}

	// test myForAll is skipped due to hints for 1st questions ^_^

	test("Q13 - lastElemAPI") {
		assertResult(Some("yes")) {
			lastElemAPI(List("no", "yes", "no", "no", "yes"))
		}
	}

	test("Q14 - appendAPI") {
		assertResult(List(1,3,5,2,4)) {
			appendAPI(List(1,3,5), List(2,4))
		}
	}

	test("Q15 - intListAPI a == b") {
		assertResult(List(6)) {
			intListAPI(6,6)
		}
	}

}