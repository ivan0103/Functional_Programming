package basics

/**
 * PART 1 - BASICS - FUNCTIONS AND LAMBDAS
 *
 * This part teaches you about writing (anonymous) functions in Scala.
 * It is worth 10 points.
 */
object Part1 {

  /**
   * An example to show what function definitions look like in Scala.
   *
   * @param i a number
   * @return i * 5 + 2
   */
  def increaseGrade(i: Int): Int = {
    val temp = i * 5
    return temp + 2
  }

  def main(args: Array[String]) = {
    println(leapYear(1990))
    println(question2())
  }

  /**
   * Scala supports both val/var variables (There is actually one more type,
   * but that is outside of the scope of this course).
   * The difference is that once you assign a value to a `val` it cannot be
   * reassigned. If you would use `var` you can reassign its value though.
   * Functional Programming purists will consider var as blasphemy, but Scala
   * is a bit more pragmatic supporting both imperative programming with vars
   * and mutability as well as the functional style with vals and
   * immutability. Within this lab assignment you are *NOT* allowed to use
   * var to help speed up the functional programming learning process.
   *
   * The `return` keyword in Scala is optional:
   * the last value produced is returned automatically, in this case:
   * temp + 2
   *
   * Instead of the above method body between braces you could also write more
   * minimalistically:
   *
   * def increaseGrade(i: Int) = i*5 + 2
   *
   * The above expression can also be defined as an anonymous function and
   * assigned to a variable, i.e. the anonymous function (or lambda or lambda
   * expression as it is also called) can be written as follows:
   *
   * (i: Int) => i*5+2
   *
   * This can be assigned to variable called increasGrade by:
   *
   * val increaseGrade = (i: Int) => i*5 + 2
   *
   * So from this example you see that defining an anonymous function is done
   * by first supplying the input parameters between () separated by commas,
   * followed by a '=>', ending with the method body.
   *
   * You can apply such a function for example to a list by supplying it to
   * methods that take a function as input parameter. Be careful of the
   * correct function signature or else your compiler/interpreter/IDE
   * will complain.
   *
   * Example usage:
   * List(22,34,48).map(increaseGrade) // results in List(112, 172, 242)
   *
   * This example takes our lambda and applies it to each element of the list
   * It works because:
   * 		1. List.map takes a function as input parameter
   *       2. final def map[B](f: (A) => B): List[B] is the function signature
   *       of map. It takes a function (A) => B, a function which takes a
   *       type A element, does something to it, and returns a type Be
   *       Our anonymous function is of type Int => Int, so this works.
   */

  /** Q1 (3p)
   *
   * Write the method body for determining whether a year is a leap year
   */
  def leapYear(i: Int): Boolean = (i % 4 == 0) && (i % 100 != 0 || i % 400 == 0)

  /** Q2 (4p)
   * You've seen an example of anonymous functions in Scala.
   * Given the following two higher order functions (HOFs), i.e.
   * functions that take functions as input parameter, replace the ??? of the
   * anonymous functions with methods bodies and provide the correct
   * function signature for myFirstHof/mySecondHof in the method body of
   * question2 such that running `question2()` results in the Lists given in
   * Note1
   */

  // TODO: uncomment the following 3 vals, replace the ??? with lambda expressions (anonymous functions)
  	val isEven = (i: Int) => i%2==0 // tells whether given Int is even or not
  	val isOdd = (i: Int) => i%2==1 // same but this time if it's odd
  	val timesTwo = (i: Int) => i*2;   // takes an Int and doubles it
  //
  def question2(): List[List[Int]] = {
    def myFirstHOF(xs: List[Int], f: Int => Boolean ) : List[Int] =
      xs.filter(f)
    def mySecondHOF(xs: List[Int], f: Int => Int ) : List[Int] =
      xs.map(f)

    val first = myFirstHOF(List(1,2,3,4), isEven)
    val second = myFirstHOF(List(1,2,3,4), isOdd)
    val third = mySecondHOF(List(10,11,12,13,14,15), timesTwo)

    val finalResult = List(first, second, third)
    finalResult
  }

  /* Note 1
   * running println(question2()) results in
   * List(List(2,4), List(1,3), List(20,22,24,26,28,30))
   *
   * It is also good for you to see that you can define methods within methods
   * (if you ever need helper methods but do not want the outer world to see,
   * use this technique, i.e. it does more or less the same as defining a
   * private helper function and calling that from with the method body)
   */

  /* Note 2:
   * As you can see from inspecting the function signature of question2
   * it takes no input parameters and returns a List of List[Int]
   *
   * You don't need to specify the return type, except for recursive
   * functions; Scala will infer most return types for you. It is however good
   * practice to specify the return type when defining the function signature.
   * If you read another person's code without reading the whole method body,
   * having the complete function signature including return type is a hint to
   * its inner workings. Also, when debugging, it can really help you to view
   * the function as a blackbox with input/output types, so supplying the
   * return type has good benefits.
   */

  /** Q3 (3p)
   * given two Ints, generate a list of integers (List[Int]) containing the numbers in between and with both numbers inclusive
   *
   * Examples:
   * intList(2,7) // List(2,3,4,5,6,7)
   * intList(3,0) // List()
   *
   * Hint: take a peek at part 2 since this question bridges part 1 and part 2
   */
  // TODO: implement the function body
  def intList(a: Int, b: Int): List[Int] = {
    if (a <= b) {
      a :: intList(a + 1, b)
    } else {
      Nil
    }
  }
}
	