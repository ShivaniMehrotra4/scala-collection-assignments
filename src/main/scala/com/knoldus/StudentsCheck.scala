package com.knoldus

import scala.collection.immutable.ListMap

case class Student(id: Long, name: String)
case class Marks(subjectId: Int, studentId: Long, marksObtained: Float)

object StudentsCheck extends App{
  val listStudent: List[Student] = List(
    Student(1, "Kunal"),
    Student(2, "Himanshu"),
    Student(3, "Geetika"),
    Student(4, "Anmol"),
    Student(5, "Shivani"),
    Student(6, "Sapna"),
    Student(7, "Mansi"),
    Student(8, "Krishna"),
    Student(9, "Umang"),
    Student(10, "Muskan")
  )

  val listMarks: List[Marks] = List(
    Marks(1, 1, 57), Marks(2, 1, 65), Marks(3, 1, 67), Marks(4, 1, 34), Marks(5, 1, 90),
    Marks(1, 2, 89), Marks(2, 2, 91), Marks(3, 2, 45), Marks(4, 2, 75), Marks(5, 2, 80),
    Marks(1, 3, 90), Marks(2, 3, 78), Marks(3, 3, 66), Marks(4, 3, 86), Marks(5, 3, 67),
    Marks(1, 4, 87), Marks(2, 4, 89), Marks(3, 4, 99), Marks(4, 4, 99), Marks(5, 4, 98),
    Marks(1, 5, 65), Marks(2, 5, 43), Marks(3, 5, 88), Marks(4, 5, 89), Marks(5, 5, 79),
    Marks(1, 6, 70), Marks(2, 6, 78), Marks(3, 6, 87), Marks(4, 6, 79), Marks(5, 6, 97),
    Marks(1, 7, 79), Marks(2, 7, 56), Marks(3, 7, 70), Marks(4, 7, 67), Marks(5, 7, 87),
    Marks(1, 8, 87), Marks(2, 8, 24), Marks(3, 8, 87), Marks(4, 8, 95), Marks(5, 8, 45),
    Marks(1, 9, 90), Marks(2, 9, 43), Marks(3, 9, 95), Marks(4, 9, 89), Marks(5, 9, 57),
    Marks(1, 10, 99), Marks(2, 10, 97), Marks(3, 10, 90), Marks(4, 10, 96), Marks(5, 10, 55))

  def calculatePercentage(st: Student): Float = {
    listMarks.filter(_.studentId == st.id).map(_.marksObtained).sum / 5
  }
  val percentageCalculatedMap = listStudent.map(elem => elem.name -> calculatePercentage(elem)).toMap
  /**
   * QUESTION 1
   * Input:- (subjectId, percentage, pass/fail)
   * Output:- for input pass, evaluate that how much students(id, name) are passed in the inputted subjectId
   * for input fail, evaluate that how much students(id, name) are failed in the inputted subjectId
   * Note:- percentage is the input which defines the minimum passing criteria
   *   e.g.
   * Pass count: 15
   * Fail count: 10
   *
   * @param subId -  takes subjectId as input
   * @param percentage - takes percentage as passing criteria
   * @param status - represents top or bottom set of rankers.
   * @return - prints the string containing counts as specified
   */


  def functionOne(subId: Int, percentage: Float, status: String): String = {
    if (status.toLowerCase == "pass") {
      val answer1 = for (listMarksTemporary <- listMarks; listStudentTemporary <- listStudent
           if listMarksTemporary.subjectId == subId && listMarksTemporary.marksObtained >= percentage
           if listMarksTemporary.studentId == listStudentTemporary.id
           ) yield listStudentTemporary
      val answer1Length = answer1.length
      s" Pass Count : $answer1Length"
    }
    else if(status.toLowerCase == "fail"){
      val answer2 = for (listMarksTemporary <- listMarks; listStudentTemporary <- listStudent
           if listMarksTemporary.subjectId == subId && listMarksTemporary.marksObtained < percentage
           if listMarksTemporary.studentId == listStudentTemporary.id
           ) yield listStudentTemporary
      val answer2Length = answer2.length
      s" Pass Count : $answer2Length"
    }
    else
      {
        "NOT ALLOWED"
      }
  }
  println(functionOne(4,80,"PaSS"))

  /**
   * QUESTION-2
   * Input:- (subjectId, count, top/bottom)
   * Output:- based on the last input(top/bottom), output the students details who have scored max/min in that subjectId
   *
   * @param subId - takes subjectId as input
   * @param count - count specifies the number upto which elements are to be displayed
   * @param level - represents top or bottom set of rankers.
   * @return - returns a ListMap
   */
  def functionTwo(subId: Int, count: Int, level: String): String = {
    val a = for {
      listMarksTemporary <- listMarks
      listStudentTemporary <- listStudent
      if listMarksTemporary.subjectId == subId && listMarksTemporary.studentId == listStudentTemporary.id
    } yield listStudentTemporary.name -> listMarksTemporary.marksObtained

    if (level.toLowerCase.equals("top")) {
      val result1 = ListMap(a.sortWith(_._2 < _._2): _*)
      result1.take(count).toString()
    }
    else if (level.toLowerCase.equals("bottom")) {
      val result2 = ListMap(a.sortWith(_._2 > _._2): _*)
      result2.take(count).toString()
    }
    else
                   "invalid"

  }

  println(functionTwo(4,3,"top"))
  println(functionTwo(2,4,"bottom"))
  println(functionTwo(2,4,"boom"))

  def calculatePercentageMinOrMax(st: Student): Float = listMarks.filter(_.studentId == st.id).map(_.marksObtained).sum / 5

  /**
   * Input:-
   * (top/bottom, count)
   * OutPut:-
   * Overall top/least scorer based on all the subjects score, fetch students name
   * count- input defines that how much students name are to be printed on console
   *
   * @param level -represents top or bottom set of rankers.
   * @param count- count specifies the number upto which elements are to be displayed
   */
  def MinOrMax(level: String, count: Int): String = {


    val a = listStudent.map(temp => temp.name -> calculatePercentageMinOrMax(temp))
    if (level.toLowerCase.equals("top")) {
      ListMap(a.sortWith(_._2 < _._2): _*).take(count).toString()
    }
    else if(level.toLowerCase.equals("bottom")){
      ListMap(a.sortWith(_._2 > _._2): _*).take(count).toString()
    }
    else
    {
      "INVALID INPUT"
    }

  }

  println(MinOrMax("top",2))

  /**
   * Input:-
   * (percentage, good_scholarship, normal_or_no_scholarship)
   * Output:- two groups of students with the amount of scholarship
   *
   * @param percentage - takes percentage as passing criteria
   * @param goodScholarship - if student's percentage is >= passing criteria , more scholarship
   * @param normalOrNoScholarship - if student's percentage is < passing criteria , less scholarship
   * @return - list of students along with their scholarships status
   */
  def functionFour(percentage: Float, goodScholarship: Long, normalOrNoScholarship: Long): List[(String, Long)] = {

    def funcFourPart1(percentage: Float, goodScholarship: Long): List[(String, Long)] =
      for (listStudentTemporary <- listStudent
           if percentageCalculatedMap(listStudentTemporary.name) >= percentage
           ) yield (listStudentTemporary.name, goodScholarship)

    def funcFourPart2(percentage: Float, normalOrNoScholarship: Long): List[(String, Long)] =
    {
      for (listStudentTemporary <- listStudent
           if percentageCalculatedMap(listStudentTemporary.name) < percentage
           ) yield (listStudentTemporary.name, normalOrNoScholarship)
    }

    val l1 :List[(String,Long)]= funcFourPart1(percentage,goodScholarship)
    val l2 :List[(String,Long)]= funcFourPart2(percentage,normalOrNoScholarship)
    l1 ::: l2
  }
  println(functionFour(80,2000,500))

  /**
   *
   * @param status represents pass or fail
   * @param percentage - passing criteria
   * @return - list
   */
  def functionFive(status: String, percentage: Float): List[(String, Float)] = {

    if (status.toLowerCase == "pass") {
      for (listStudentTemporary <- listStudent
           if percentageCalculatedMap(listStudentTemporary.name) >= percentage
           ) yield (listStudentTemporary.name, percentageCalculatedMap(listStudentTemporary.name))
    }
    else  {
      for (listStudentTemporary <- listStudent
           if percentageCalculatedMap(listStudentTemporary.name) < percentage
           ) yield (listStudentTemporary.name, percentageCalculatedMap(listStudentTemporary.name))
    }
  }

  println(functionFive("fail",80))

  /**
   *
   * @param percentage - passing criteria
   * @return - list
   */
  def functionSix(percentage: Float): List[(String, Float)] = {

    for (listStudentTemporary <- listStudent
         if percentageCalculatedMap(listStudentTemporary.name) >= percentage
         ) yield (listStudentTemporary.name, percentageCalculatedMap(listStudentTemporary.name))
  }

  println(functionSix(70))


}






