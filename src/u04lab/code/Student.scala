package u04lab.code

//import Lists._

import u04lab.code.Lists.List
import u04lab.code.Lists.List._

trait Student {
  def name: String

  def year: Int

  def enrolling(course: Course*): Unit // the student participates to a Course

  def courses: List[String] // names of course the student participates to

  def hasTeacher(teacher: String): Boolean // is the student participating to a course of this teacher?
}

trait Course {
  def name: String

  def teacher: String
}

object Student {
  def apply(name: String, year: Int = 2017): Student = StudentImpl(name, year)

  case class StudentImpl(override val name: String, override val year: Int) extends Student {

    private var list: List[Course] = Nil()

    override def courses: List[String] = map(list)(x => x.name)

    override def hasTeacher(teacher: String): Boolean = find(map(list)(x => x.teacher))(teacher)

    override def enrolling(course: Course*): Unit = for (i <- course) {
      list = Cons(i, list)
    }
  }
}

object Course {
  def apply(name: String, teacher: String): Course = CourseImpl(name, teacher)

  case class CourseImpl(override val name: String, override val teacher: String) extends Course {

  }
}

object sameTeacher {
  def unapply(course: List[Course]): Option[String] = {
    val teachers = map(course)(a => a.teacher)
    val first: String = drop(teachers,1) match {
      case Cons(h,t) => h
    }
    if (allEquals(teachers)(first)) Option(first) else Option.empty
  }

  def find(list: List[String], elem: String): Option[String] = {
    if (foldLeft(list)(true)((a,b) => (elem == b) && a)) Option(elem) else Option.empty
  }
}

object Try extends App {
  val cPPS = Course("PPS", "Viroli")
  val cPCD = Course("PCD", "Ricci")
  val cSDR = Course("SDR", "D'Angelo")
  val cSDR2 = Course("SDR2", "D'Angelo")
  val cSDR3 = Course("SDR3", "D'Angelo")
  val s1 = Student("mario", 2015)
  val s2 = Student("gino", 2016)
  val s3 = Student("rino") //defaults to 2017
  s1.enrolling(cPPS)
  s1.enrolling(cPCD)
  s2.enrolling(cPPS)
  s3.enrolling(cPPS, cPCD, cSDR)
  /*s3.enrolling(cPPS)
  s3.enrolling(cPCD)
  s3.enrolling(cSDR)*/
  println(s1.courses, s2.courses, s3.courses) // (Cons(PCD,Cons(PPS,Nil())),Cons(PPS,Nil()),Cons(SDR,Cons(PCD,Cons(PPS,Nil()))))
  println(s1.hasTeacher("Ricci")) // true

  val coursesRight = List(cSDR, cSDR2, cSDR3)
  coursesRight match {
    case sameTeacher(t) => println(s"$coursesRight have same teacher $t")
    case _ => println(s"$coursesRight have different teachers ")
  }
  val coursesWrong = List(cPPS, cPCD, cSDR)
  coursesWrong match {
    case sameTeacher(t) => println(s"$coursesWrong have same teacher $t")
    case _ => println(s"$coursesWrong have different teachers ")
  }
}

/** Hints:
 * - simply implement Course, e.g. with a case class V
 * - implement Student with a StudentImpl keeping a private Set of courses V
 * - try to implement in StudentImpl method courses with map V
 * - try to implement in StudentImpl method hasTeacher with map and find V
 * - check that the two println above work correctly V
 * - refactor the code so that method enrolling accepts a variable argument Course* V
 */
