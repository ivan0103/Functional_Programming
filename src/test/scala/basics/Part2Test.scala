package basics

import basics.Part2._
import org.scalatest.FunSuite

class Part2Test extends FunSuite {

	test("Sum") {
		assertResult(15) {
			sum((1 to 5).toList)
		}
	}

	test("Optional sum") {
		assertResult(16) {
			optionalSum(List(Num(2), Nothing(), Num(5), Num(3), Nothing(), Num(2), Nothing(), Nothing(), Num(4)))
		}
	}

	test("traffic light example as test") {
		assertResult(
			List(
				(false,"ok: nothing wrong here"),
				(false,"ok: nothing wrong here"),
				(false,"ok: we allow it"),
				(true,"ok: too late"),
				(true,"ok: waaay too late"),
				(false,"ERROR: (bananas,1100123)")
			)) {
			val drivers = List(
				("green", 14),
				("green", 3),
				("orange", 2),
				("orange", 6),
				("red", 1),
				("bananas", 1100123))

			drivers.map(arg => trafficLightPenalty(arg._1, arg._2))
		}
	}

	test("Q4 - twice") {
		assertResult(List("abc", "abc")) {
			twice(List("abc"))
		}
	}

	test("Q5 - drunkWords") {
		assertResult(List("uoy","yeh")) {
			drunkWords(List("hey", "you"))
		}
	}

	// test myForAll is skipped due to hints for 1st questions ^_^

	test("Q7 - lastElem") {
		assertResult(Some("yes")) {
			lastElem(List("no", "yes", "no", "no", "yes"))
		}
	}

	test("Q8 - append") {
		assertResult(List(1,3,5,2,4)) {
			append(List(1,3,5), List(2,4))
		}
	}

	// test myFilter is skipped due to hints for 1st questions ^_^
	test("Q9 - filter") {
		assertResult(List(0,4, 8)) {
			myFilter(List.range(0,11), (i: Int) => i % 2 == 0)
		}
	}
}