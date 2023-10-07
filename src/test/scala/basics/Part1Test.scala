package basics

import basics.Part1._
import org.scalatest.FunSuite

class Part1Test extends FunSuite {

	// Using `assert` you can make assertions in tests.
	test("Simple math") {
		assert(increaseGrade(3) == 17, "3 * 5 + 2 = 17")
	}

	// Alternatively you can expect a certain result from an operation.
	// This gives better error messages and looks cleaner.
	test("More math") {
		assertResult(42, "8 * 5 + 2 = 42") {
			increaseGrade(8)
		}
	}

	test("Q1 - true leapYear") {
		assertResult(true, "Normal leapyear is not correctly determined") {
			leapYear(1984)
		}
	}

	test("Q2 - lambda") {
		assertResult(true, "lambda function definition is incorrect") {
			isEven(4)
		}
	}

	test("Q2 - HOFS function signature") {
		assertResult(List(List(2,4), List(1,3), List(20,22,24,26,28,30)), "one or both of the function types for myHOF1/2 is incorrect") {
			question2()
		}
	}

	test("Q3 - intList") {
		assertResult(List(1,2,3)) {
			intList(1,3)
		}
	}
}
