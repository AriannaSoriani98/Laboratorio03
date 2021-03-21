package u03

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u03.Streams.Stream.{Cons, Empty, cons, empty}

class TestStream {
  import u03.Streams.Stream
  import u03.Streams.Stream._

  import u03.StreamFunction._

  val s = Stream . take ( Stream . iterate (0) (_+1) ) (10)

  @Test def testDrop(): Unit ={
    assertEquals(toList(cons(6,cons(7,cons(8,cons(9,empty()))))),toList(drop(s)(6)))
  }

  @Test def testConstant(): Unit ={
    assertEquals(toList(cons("x",cons("x",cons("x",cons("x",cons("x",empty())))))),toList ( take ( constant ("x")) (5)))
  }

  @Test def testFibonacci(): Unit ={
    println(toList(take ( fib ) (20)))
  }

}
