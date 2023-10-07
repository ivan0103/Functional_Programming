package basics

/*
 * PART 3 - RINSE AND REPEAT - THE POWER OF THE API
 *
 * Ok, good, you've finished part two and now it's on to the next thing,
 * but no good practice without seeing there are many ways to slice a cake
 *
 * You just finished implementing myFilter in part 2. Below are two ways you 
 * could do the same using methods from the standard library (i.e. API methods)
 * There are developers who strive for oneliners, i.e.
 *
 * val specialEvenNumbers1 = List.range(0,11).filter(isEven(_)).zipWithIndex.filter(x => x._2 %2 == 0).map(tuple => tuple._1)
 *   or...
 * val specialEvenNumbers2 = (List.range(0,11).groupBy(x => x % 2 == 0) get true).get.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
 *
 * WAIT ... WHUT? 
 * While correct for beginning Scala developers this is torture... 
 *
 * After some time you'll get used to this, but there will be many times you
 * will want to pull your hair out debugging the incomprehensible messages 
 * from the Scala compiler.
 *
 * How do you combat this/undertake questions to end up with a one line result?
 *
 * Well, that's easy: it takes a bit of practice, but remember the 
 * Unix pipelines? We're going to do the same.
 *
 * If you use IntelliJ you'll have the advantage that types will be placed in 
 * comments popups at the right side of your code, but if that's not working for 
 * some reason, we're going to add it in comments by hand. I'll go over the 2 
 * examples from above and show what I mean.
 *
 * val specialEvenNumbers1 = List   // put the next method call on the next line with a .
 *   .range(0, 11)             		// range(0,11) returns List(1,2,3,4,...,10) List.range 2nd arg is exclusive
 *   .filter(isEven(_))            	// provide a lambda expression to retain all list elements which are even, i.e. List(0,2,4,6,8,10)  
 *   .zipWithIndex                 	// creates Tuple2[Int, Int] of each list element with its index starting at 0, i.e. List((0,0),(2,1), (4,2), (6,3), (8,4), (10,5))
 *   .filter(tup => tup._2 %2 == 0) // filter the tuples: retain all elements for which the 2nd element of the tuple, the index, is even: List((0,0), (4,2), (8,4))
 *   .map(tuple => tuple._1)       	// map over the tuple and just retain the first element of each tuple List(0,4,8)
 *
 * 
 * val specialEvenNumbers2 = List
 *   .range(0,11)					// again get List(0,1,2,...,10)
 *   .groupBy(x => x % 2 == 0) get true)    
 *   								// groupBy(lambda) transforms into 
 *										HashMap(
 *											false -> List(1, 3, 5, 7, 9), 
 *											true -> List(0, 2, 4, 6, 8, 10)
 *										)
 *   // then calling `get true` on that HashMap gets you an Option of the key
 * 	 // so if the map has results for key 'true' you get Some(List(0,2,4,6,8,10) 
 *   .get                           // unwraps the Option. If you had a None and called .get you get an exception!!
 *  								// read more on Option in the Scala Option API 
 *   .zipWithIndex					// like above
 *   .filter(_._2 % 2 == 0)         // shorthand lambda which functions just like above
 *   .map(_._1)                     // shorthand lambda/tuple notation
 *
 * so the idea is to put every method call after the `.` on a next line 
 * and type in comments the result type or sometimes even just the result on the
 * right in comments, i.e.
 *
 * someVal.someMethod 		// List[(a:Int, b:String)]
 *  .map(lambda)			// List[Int]
 *  .sum					// Int
 *
 * By writing your code like this, you can go over it step by step, and check
 * each transformation, just like a Unix pipeline. This will be beneficial
 * if you reach the Spark/Flink assignments
 *
 * Now enough of this... Time to get your hands dirty.
 * 
 * You are asked to repeat the exact same functions from part 2 
 * (except myFilter), and you are also asked for intlist from part 1,
 * but this time ONLY using standard scala API methods (mostly from List)
 * And NO... still no `vars` allowed!!! :)
 * 
 * This part is worth 12 points
 */
object Part3 {
  /** Q10 (2p)
   * TODO: implement `twice` only using API calls
   */
  def twiceAPI[A](xs: List[A]): List[A] = xs.flatMap(x => List(x, x))

  /** Q11 (2p)
   * TODO: implement `drunkWords` only using API calls
   */
  def drunkWordsAPI(xs: List[String]) = xs.reverse.map(x => x.reverse)

  /** Q12 (2p)
   * TODO: implement `myForAll` only using API calls
   */
  def myForAllAPI[A](xs: List[A], f: A => Boolean): Boolean = xs.map(x => f(x)).forall(y => y);

  /** Q13 (2p)
   * implement `lastElem` only using API calls
   */
  def lastElemAPI[A](xs: List[A]): Option[A] = {
    if (xs.isEmpty) {
      None
    } else {
      Some(xs.reverse.head)
    }
  }

  /** Q14 (2p)
   * implement `append` only using API calls
   */
  def appendAPI[A](xs: List[A], ys: List[A]): List[A] = xs.foldRight(ys)(_ :: _)

  /** Q15 (2p)
   * implement `intList` (from part 1) only using API calls
   */
  def intListAPI(a: Int, b: Int) = {
    if (a <= b) {
      (a to b).toList
    } else {
      Nil
    }
  }
}