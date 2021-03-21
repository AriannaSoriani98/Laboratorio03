package u03

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u02.Lists.List.{cons, empty, of}
import u02.Lists.List._



class TestFold {
  import u02.Lists._
  import u03.Fold._

  val lst: List[Int] = cons (3, cons (7, cons (1, cons (5, empty()))))

  @Test def testFoldLeftAndRight(): Unit ={
    assertEquals(-16,foldLeft (lst)(0)(_-_))
    assertEquals(-8,foldRight (lst)(0)(_-_))
  }

}
