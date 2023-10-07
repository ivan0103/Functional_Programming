package functions

import Part4._
import org.scalatest.FunSuite

class Part4Test extends FunSuite {

	val asr1 = List(// list of student tuples with (name, course, Option[result])
		("a", "bdp", Some(8)), ("a", "ci", Some(9)), ("a", "dm", None),
		("b", "bdp", Some(7)), ("b", "ci", Some(7)), ("b", "dm", Some(7)),
		("c", "bdp", Some(6)), ("c", "ci", Some(3)), ("c", "dm", Some(9)),
		("d", "bdp", Some(4)), ("d", "ci", Some(9)), ("d", "dm", Some(3)),
		("e", "bdp", Some(7)), ("e", "ci", Some(8)), ("e", "dm", Some(8)),
		("f", "bdp", Some(6)), ("f", "ci", Some(6)), ("f", "dm", Some(8)),
		("g", "bdp", Some(6)), ("g", "ci", Some(6)), ("g", "dm", Some(9))
	)

	val ss1 = List("a", "d", "f","g")

	test("Q16 - reverse using foldLeft") {
		assertResult(List(1, 0, -1, -2, -3)) {
			reverseUsingFold(List.range(-3,2))
		}
	}

	test("Q17 - 2nd largest") {
		assertResult(Some(5)) {
			secondLargest(List(1,2,3,4,5,6))
		}
	}

	test("Q18 - countnumbers") {
		assertResult(Map(1 -> 6)) {
			countNumbers(List(1,1,1,1,1,1))
		}
	}
	test("Q19 - distinct") {
		assertResult(List("A", "B", "C", "D")) {
			distinctAPI(List("A", "B", "C", "D","A", "B", "C", "D","A", "B", "C", "D"))
		}
	}

	test("Q20 - special partition") {
		assertResult(true) {
			specialPartition(
				List(
					List(-4,1,2),
					List(-4,-4,-6,1,2,4,4),
					List(-4,-4, -6, 1,2,8)
				)
			)
		}
	}

	test("Q21 - good players") {
		assertResult(List("Ola")) {
			determineGoodPlayers(
				List(
					("Ola", List(("abcc",3), ("accc",9), ("acdc",5), ("aecc",4))),
					("Georgios", List())
				)
			)
		}
	}

	test("Q22 - average over 7") {
		assertResult(List(("Ola",7.2), ("Burçu",9.0), ("Georgios",7.333333333333333))) {
			val pl: List[(String, List[(String, Int)])] = List(
				("Ola", List( ("tree", 4), ("plant",12), ("water", 10), ("earth",9), ("heaven",1))),
				("Liudas", List( ("hop", 3), ("editor", 7), ("mileage", 8), ("load", 4), ("quintuple", 16))),
				("Roald", List( ("language", 8), ("three", 5), ("hip", 3))),
				("Burçu", List( ("beverage",11), ("institution",14), ("player",8), ("feedback",7), ("positive",9), ("login",5))),
				("Georgios", List( ("functional", 9), ("program", 7), ("facebook", 6)))
			)
			avgOver7(pl)
		}
	}

	test("Q23 - good sport students") {
		assertResult(List( "g","f")) {
			goodSportStudents(asr1, ss1)
		}
	}

	test("Q24 - avg grade") {
		assertResult(List(("g",7.0),("f",6.666666666666667))) {
			avgGradeGoodSportStudents(asr1, ss1)
		}
	}
}