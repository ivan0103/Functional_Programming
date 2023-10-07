package dataset

/**
 * PART 5A - DATASET1 / REAL LIFE APPLICATION
 *
 * In the following questions you will solve realistic problems with the 
 * techniques you learned in this assignment. You will be working with data of 
 * San Francisco Library patrons.
 *
 * Below you can find what each field means.
 * Id: Id of patron
 * Patron Type Definition: Description of patron (adult, teen, child, senior, 
 * etc.).
 * Total Checkouts: Total number of items the patron has checked out from the 
 * library since the record was created.
 * Total Renewals: Total number of times the patron has renewed checked-out 
 * items.
 * Age Range: Age ranges: 0 to 9 years, 10 to 19 years, 20 to 24 years, 
 * 25 to 34 years, 35 to 44 years, 45 to 54 years, 55 to 59 years,
 * 60 to 64 years 65 to 74 years, 75 years and over. Some blank.
 * Home Library Definition: Description of the branch library where the 
 * patron was originally registered.
 * Circulation Active Month: Month the patron last checked out library 
 * materials, or last logged into the library’s subscription databases
 * from a computer outside the library.
 * Circulation Active Year: Year the patron last checked out library materials, 
 * or last logged into the library’s subscription databases from a computer
 * outside the library.
 * Notice Preference Definition: Description of the patron’s preferred method of
 * receiving library notices.
 * Provided Email Address: Indicates whether the patron provided an email 
 * address.
 * Year Patron Registered: Year patron registered with library system. 
 * No dates prior to 2003 due to system migration.
 * Outside of County: If a patron's home address is not in San Francisco, then 
 * flagged as true, otherwise false.
 * Supervisor District: Based on patron address: San Francisco Supervisor 
 * District. Note: This is an automated field, please note that if
 * "Outside of County" is true, then there will be no supervisor district.
 * Also, if the input address was not well-formed, the supervisor district
 * will be blank.
 *
 * Solve the following questions using functional programming. 
 * The code for reading the file is already given.
 *
 * This part is worth 9 points
 */
object Part5A {

  def main(args: Array[String]): Unit = {
    val patrons = scala.io.Source.fromResource("library-first6000.csv").getLines().toList.drop(1).map(line => line.split(","))

    // use this if you want nice output to fiddle around a bit
    println(patrons.head.toList)
    println(exampleOutput(patrons))
  }

  /** gets you the ids of the patrons and their age */
  def exampleOutput(xs: List[Array[String]]): List[(Int, String)] = xs.map((x: Array[String]) => (x(0).toInt, x(4)))

  /** Q25 (3p)
   * Count the number of patrons who didn't provide an age to the library at all
   *
   * @param xs the list of patrons
   * @return the number of people who didn't supply their age
   */
  def noAge(xs: List[Array[String]]): Int = xs.count(x => x(4).isEmpty)
  /** Q26 (3p)
   * Output the id, age range and notice preference as a tuple of the patron with the highest number of checkouts
   */
  def highestCheckouts(xs: List[Array[String]]): (Int, String, String) = {
    val data = xs.map { arr =>
      val id = arr(0).toInt
      val ageRange = arr(4)
      val noticePreference = arr(8)
      val checkouts = arr(2).toInt
      (id, ageRange, noticePreference, checkouts)
    }
    val patronWithHighestCheckouts = data.maxBy(_._4)
    (patronWithHighestCheckouts._1, patronWithHighestCheckouts._2, patronWithHighestCheckouts._3)
  }

  /** Q27 (3p)
   * Output the patron type and number of patrons as a map and make sure the patrons satisfy the following condition:
   * - Patrons who indicated to be contacted by email but have not provided that email
   */
  def missing_email(xs: List[Array[String]]): Map[String, Int] = {
    val data = xs.map { arr =>
      val patronType = arr(1)
      val contact = arr(8)
      val providedEmail = arr(9)
      (patronType, contact, providedEmail)
    }
    val patronsWithEmail = data.collect { case (patronType, "email", "false") => patronType}
    patronsWithEmail.groupBy(identity).mapValues(_.size)
  }
  // END OF PART 5A
}
