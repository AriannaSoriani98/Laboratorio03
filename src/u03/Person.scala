package u03

import u02.SumTypes
import u02.SumTypes.{Student, Teacher}
import u03.Lists.List
import u03.Lists.List.{Cons, Nil, append}

object Person {

  import SumTypes.Person
  import Lists.List.map
  import Lists.List.filter

  var student1: Person = Student("Arianna", 22)
  var student2: Person = Student("Luca", 22)
  var teacher1: Person = Teacher("Mirko", "pps")
  var teacher2: Person = Teacher("Roberto", "pcd")

  def empty[A](): List[A] = Nil()

  def cons[A](h: A, t: List[A]): List[A] = Cons(h, t)

  var listOfPersons: List[Person] = cons(student1, cons(teacher2, cons(student2, empty())))

  def appendAPerson[A](l1: List[A], l2: A): List[A] = l1 match {
    case Cons(h, t) => Cons(h, appendAPerson(t, l2))
    case _ => l1
  }

  def getCourses(listOfPersons: List[Person]): List[String] = listOfPersons match {
    case Cons(h, t)=> h match {
      case Teacher(_, course) => Cons(course, getCourses(t))
      case Student(_, _) => getCourses(t)
    }
    case Nil() => Nil()
  }





  }
