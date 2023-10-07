package basics

/**
 * PART 2 - LISTS AND PATTERN MATCHING
 *
 * It is worth 18 points.
 */
object Part2 {

  // Run this main function to see the result of the `println` calls.
  def main(args: Array[String]): Unit = {
    // In Scala, lists are defined recursively. A list consisting only of
    // the item 1 looks like this:
    val one = 1 :: Nil // can also be created by calling List(1)

    /**
     * The `::` operator concatenates an item and a list.
     * The `::` prepends an item to a list in O(1); There is also an
     * append, but we highly discourage its use since it's O(n)
     *
     * As the right side always needs to be a list, the terminating value
     * `Nil` is added. `Nil` is the same as an empty List: List()
     *
     * You could read the following list as `1 :: (2 :: (3 :: (Nil)))`,
     * every part in brackets is a list and the method `::` is called on
     * that list, i.e. `::` is called on its right operand.
     */
    val three = 1 :: 2 :: 3 :: Nil
    // Nil can be replaced by List(), but using `::` Nil seems tradition

    /**
     * For 1 chain in the list `x :: y`, `x` is usually referred to as the
     * head of the list, `y` is called the tail. They can be accessed
     * as follows:
     */
    val h = three.head
    val t = three.tail
    println(s"Head of $three is $h")
    println(s"Tail of $three is $t")

    /* Note 1
     * the two println statements above use something called String
     * interpolation in Scala: You start with s"..." and between the quotes
     * whenever you need to supply a value, use $variable_name to get it
     */

    /**
     * Since the lists are defined recursively, accessing a certain index
     * is not O(1) but O(n). Traversing the full list however is still O(n)
     * and this is what is used most often in functional programming.
     *
     * Scala's lists are immutable. Any operation that should change a
     * value will return a new list. For example, increasing the value of
     * every item with 1 looks like this:
     */

    val plusOne = three.map(x => x + 1)
    println(three) // The original list is not changed
    println(plusOne) // This is a new list


    /**
     * Lists can also be created with the `List` function. This is a
     * shorthand for the recursive way given earlier. When printing lists
     * to the console, they are displayed like this as well.
     */
    val four = List(1, 2, 3, 4)

    // calling `println(sum(four))` displays the result of the function sum
    // which is defined using pattern matching below
    println(sum(four)) // 10

    val drivers = List(
      ("green", 14),
      ("green", 3),
      ("orange", 2),
      ("orange", 6),
      ("red", 1),
      ("bananas", 1100123))

    /*
     * the output of calling the traffic light function on drivers
     * N.B. the type signature of the function and how the input is mapped
     */
    println(drivers.map(arg => trafficLightPenalty(arg._1, arg._2)))

  }

  /**
   * Recursively sums all values in the list.
   *
   * @param xs the list to process.
   * @return the sum of all values in xs.
   *
   */
  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case i :: tail => i + sum(tail)
  }

  /* Note 1
   * Using `xs match` we "match" the value of the list. This is called pattern
   * matching. For matching the whole list without any special conditions
   * there are only two cases: Nil (or List()) and head :: tail, where head
   * is one element, tail is a list
   *
   * For the above `sum` it holds that the base case is the empty list,
   * which should return 0. If the list has a number, add the value of the
   * head to the sum of the tail
   *
   * N.B. Pay special attention to the following:
   * case matching can have a large number of cases; the FIRST one that
   * matches the argument mentioned before `match` is the one that will
   * `fire`. So if you have condition checks (guards, as they are called
   * in Scala) or a long list of cases to match against. beware of the order.
   *
   * Again:
   * 		!!!!! THE ORDER MATTERS IN CASE MATCHING !!!!!
   */

  /*
   * The following method shows a bit more challenging example. It is supposed
   * to represent the method to determine whether a driver will receive a fine
   * when crossing a traffic light. When the light is green, or when it is
   * orange and the time passed since crossing is less than or equal to three
   * seconds passed the driver will not receive a fine. When it is red or
   * more than three seconds after the light changed to orange, the driver
   * will receive a fine.
   *
   * The return type will indicate whether the driver gets a fine, and what
   * the reason is, as a String. This tuple, i.e. (true or false, reasoning)
   * will also indicate if the traffic light is malfunctioning. Take a look
   * below for the complete example.
   */
  def trafficLightPenalty(status: String, time: Int): (Boolean, String) =
    (status, time) match {
      case ("green", _) => (false, "ok: nothing wrong here")
      case ("orange", x) if x <= 3 => (false, "ok: we allow it")
      case ("orange", y) if y > 3 => (true, "ok: too late")
      case ("red", _) => (true, "ok: waaay too late")
      case other => (false, "ERROR: " + other)
    }

  /**
   * Scala also has immutable classes, called case classes. You can use
   * pattern matching on them as well. Note that `OptionalNum` itself cannot
   * be instantiated (as it is abstract). It's either `Nothing()`, indicating
   * no value, or `Num(i)`, indicating a value.
   * the keyword `sealed` indicates that ONLY the two case classes below can
   * be an OptionalNum. This can be used as a safety mechanism when checking
   * pattern matching. With `sealed` forgetting a case in the pattern match
   * will result in the compiler producing a non-exhaustive matching warning
   * without the keyword, this will not happen.
   *
   * Scala has similar built-in classes:
   * `Option`, extended by `None()` and `Some(v)`.
   * This is a very useful one, since in stead of `null` in Java (and the
   * Exceptions that come with it, when something is not found for example
   * you can just return None without throwing an exception.
   */
  sealed abstract class OptionalNum()

  case class Nothing() extends OptionalNum

  case class Num(i: Int) extends OptionalNum

  /**
   * Returns the sum of all defined numbers in a list of optional values.
   *
   * @param xs list of optional numeric values.
   * @return the sum of all defined numbers in `xs`.
   */
  def optionalSum(xs: List[OptionalNum]): Int = xs match {
    case Nil => 0 // the base case, List()
    case Num(x) :: t => x + optionalSum(t) // h :: t case 1
    case Nothing() :: t => optionalSum(t) // h :: t case 2
  }

  /* Note that the `xs match` can be placed right after the function
   * definition. Case classes allow for pattern atching. Instead of an if
   * statement to check. Remember the sum method? Lists have normally only
   * two cases, `Nil` or `head :: tail`, but in this case we have three!
   */


  // PART 2: EXERCISES - LISTS AND PATTERN MATCHING


  /* We hope you've seen enough examples to repeat the technique on your own
   * The point to take away is that pattern matching is really if else on
   * steroids

   * For the exercises in this part you are _not_ allowed to use library
   * functions. Do not use iteration, write recursive functions instead.
   */

  /** Q4 (2p)
   * Twice takes a list and duplicates each element
   *
   * @param xs list to map
   *
   *           Example: twice(List.range(0,4)) // List(0, 0, 1, 1, 2, 2, 3, 3)
   */
  //TODO: supply the method body of twice, using pattern matching
  def twice[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case h :: t => h :: h :: twice(t);
  }

  /** Q5 (2p)
   * You had a few drinks too much after a party and recorded a message for
   * someone, but due to pushing a few buttons on your app now not only the
   * list of words that you speak is reversed, but the words itself are as
   * well
   *
   * Example:
   * drunkWords(List("Hey","you,","how","are","you","doing?"))
   * turns into
   * List("?gniod","ouy","era","woh",",ouy","yeH")
   */
  // TODO: supply the method body of drunkWords using pattern matching
  def drunkWords(xs: List[String]): List[String] = xs match{
    case Nil => Nil;
    case h :: t => drunkWords(t) :+ drunkWord(h);
  }

  def drunkWord(xs: String): String = xs.toList match {
    case Nil => ""
    case head :: tail => drunkWord(tail.mkString) + head
  }

  /** Q6 (3p)
   * MyForAll takes a list of elements, and applies a function to it, to
   * check if some condition holds. An empty list evaluates to true.
   *
   * Examples:
   * val startsWithS = (s: String) => s.startsWith("s") // lambda expression
   *
   * myForAll(List("abc", "def"), startsWithS) 					// false
   * myForAll(List("start", "strong", "system"), startsWithS) 	// true
   */
  // TODO: supply the method body of myForAll using pattern matching.
  def myForAll[A](xs: List[A], f: A => Boolean): Boolean = xs match {
    case Nil => true;
    case h :: t => {
      if (f(h)){
        myForAll(t, f);
      }
      else {
        false;
      }
    }
  }

  /** Q7 (3p)
   * This is the first question where you encounter the Option[T] type
   * Use this type in the method body of lastElem, which returns an Option[A]
   * of the last element of the given List[A]
   *
   * @param xs the list to map over
   * @return None if the list is empty or Some( .. : A), the last element of
   *         the list
   *
   *         Examples:
   *         lastElem(List()) // None
   *         lastElem(List.range(0,3)) // Some(2) (range has exclusive ceiling)
   */
  // TODO implement using pattern matching
  def lastElem[A](xs: List[A]): Option[A] = xs match {
    case Nil => None;
    case h :: Nil => Some(h);
    case h :: t => lastElem(t);
  }

  /** Q8 (3p)
   * Take two lists and concatenate them, returning the result
   *
   * @param xs , ys, the list to concatenate
   * @return the result of first all elements from xs with all elements
   *         from ys appended
   *
   *         Examples:
   *         append(List(), List()) 			// List()
   *         append(List(1,3,5), List(2,4)) 	// List(1,3,5,2,4)
   */
  // TODO implement using pattern matching
  def append[A](xs: List[A], ys: List[A]): List[A] = xs match {
    case Nil => ys
    case h :: t => h :: append(t, ys)
  }

  /**
   * Q9 (5p)
   * This question is a variant on a filter function. Given a List[A] and a
   * function f: A => Boolean, `myFilter` should retain all elements from
   * the list which satisfy 'f' and throw out all other elements, but...
   * ... it has a twist: It should also throw out each even indexed list
   * element which satisfy 'f'
   *
   * Take a look at the examples to see more directly what it needs to do
   * if you find this description vague.
   *
   * You are required to solved this using pattern matching on lists.
   * HINT: define a 'helper' method within myFilter which uses case matching.
   *
   * Examples:
   * val nrs = List.range(0,11) // List(0,1,2,3,...,10)
   * myFilter(nrs, (i: Int) => i % 2 == 0) // List(0,4,8)
   *
   * so although 2, 6 and 10 satisfy the function, they are thrown out.
   */
  // TODO implement method using pattern matching
  // a helper method which you've written yourself
  def myFilter[A](xs: List[A], f: A => Boolean): List[A] = {
    helper(filter(xs, f), 0)
  }
  def filter[A](xs: List[A], f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case h :: t =>
      if (f(h)) {
        h :: filter(t, f)
      } else {
        filter(t, f)
      }
  }
  def helper[A](remaining: List[A], index: Int): List[A] = remaining match {
    case Nil => Nil
    case h :: t =>
      if (index%2 != 0) {
        helper(t, index + 1)
      } else {
        h :: helper(t, index + 1)
      }
  }
  // END OF PART 2
}
