package dataset

import dataset.util.XMLDatafile.Badge
import scala.io.Source

/**
 * PART 5A - DATASET2 / StackOverFlow Badges
 *
 * In this assignment you will be asked to finish reading in a not quite xml file
 * The file is one big list of lines such asked
 *
 * <row Id="1" UserId="3" Name="Autobiographer" Date="2012-03-06T18:53:16.300" Class="3" TagBased="False" />
 *
 * For this assignment you first have to prep your data a bit, and then it's 
 * on to answering questions. This part is worth 9 points
 */
object Part5B {

  /** Q28 (3p)
   * Included is a first example of reading in a file:
   * `sourceAsListString` generates a `List[String]`
   *
   * We would like you to finish `source` converting this into a List of
   * case class Badge, i.e. make sure the return type for source is
   * `List[Badge], you can find Badge in the util folder
   */
  val sourceAsListString = Source.fromResource("First6200Badges.xml").getLines.toList.drop(2).dropRight(1)


  val source: List[Badge] = Source.fromResource("First6200Badges.xml").getLines().toList.drop(2).dropRight(1)
    .map { line =>
      val idPattern = """Id="(\d+)""".r
      val userIdPattern = """UserId="(\d+)""".r
      val namePattern = """Name="([^"]+)""".r
      val datePattern = """Date="([^"]+)""".r
      val classPattern = """Class="(\d+)""".r
      val tagBasedPattern = """TagBased="(\w+)""".r
      val id = idPattern.findFirstMatchIn(line).map(_.group(1).toInt).getOrElse(-1)
      val userId = userIdPattern.findFirstMatchIn(line).map(_.group(1).toInt).getOrElse(-1)
      val name = namePattern.findFirstMatchIn(line).map(_.group(1)).getOrElse("")
      val date = datePattern.findFirstMatchIn(line).map(_.group(1)).getOrElse("")
      val classValue = classPattern.findFirstMatchIn(line).map(_.group(1).toInt).getOrElse(-1)
      val tagBased = tagBasedPattern.findFirstMatchIn(line).exists(_.group(1).toBoolean)
      Badge(id, userId, name, date, classValue, tagBased)
    }

  /**
   * Again you can use this to get some output
   */
  def main(args: Array[String]): Unit = {
    println(showResults(sourceAsListString))
  }

  def showResults(input: List[String]): Unit = input.foreach(println)


  /** Q29 (3p)
   *
   * What is the easiest attainable badge? Output a tuple of its name and nr
   */
  def easiestAttainableBadge(input: List[Badge]): (String, Int) = {
    val badgeCounts = input.groupBy(_.name).mapValues(_.size)
    val mostFrequentEntry = badgeCounts.maxBy(_._2)
    (mostFrequentEntry._1, mostFrequentEntry._2)
  }
  /** Q30 (3p)
   *
   * Return a tuple of tuples of the least productive and most productive
   * year, together with the nr of badges earned
   */
  def yearOverview(input: List[Badge]): ((Int, Int), (Int, Int)) = {
    val yearCounts = input.map(badge => (badge.badgeDate.split("-")(0).toInt, 1)).groupBy(_._1).mapValues(_.size)
    val leastProductiveYear = yearCounts.minBy(_._2)
    val mostProductiveYear = yearCounts.maxBy(_._2)
    ((leastProductiveYear._1, leastProductiveYear._2), (mostProductiveYear._1, mostProductiveYear._2))
  }
  // END OF PART 5B

}
