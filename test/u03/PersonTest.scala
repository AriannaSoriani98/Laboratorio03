package u03

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u02.SumTypes.{Person, Student, Teacher}
import u03.Person.listOfPersons
import u03.Lists.List

class PersonTest {
  import u03.Person._
  val student1: Person = Student("Arianna", 22)
  val student2: Person = Student("Luca", 22)
  val teacher1: Person = Teacher("Mirko", "pps")
  val teacher2: Person = Teacher("Alessandro", "pcd")

  val listOfPersons1: List[Person] = cons(student1, cons(teacher2, cons(student2, empty())))
  val listOfPersons2: List[Person] = cons(student1, cons(teacher1, cons(student2, empty())))

  @Test def testgetCourses(): Unit ={
    assertEquals(cons("pcd",empty()),getCourses(listOfPersons1))
    assertEquals(cons("pps",empty()),getCourses(listOfPersons2))

  }

}
