package u02

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import u02.Lists.List.{cons, empty, of}
import u02.Lists.List._

class TestList {
  import u02.Lists._

  val list1: List[Int] = cons(10,cons(20,cons(30,empty())))
  val list2: List[Int] = cons(10,cons(-20,cons(30,empty())))
  val emptyList: List[Int] = empty()

  @Test def testEqualitiesOfConstruction()={
    assertEquals(of(10), cons(10,empty()))
    assertEquals(of(10,20), cons(10,cons(20,empty())))
  }

  @Test def testSum(): Unit ={
    assertEquals(60, List.sum(list1))
    assertEquals(0, List.sum(emptyList))
  }

  @Test def testSum2(): Unit ={
    assertEquals(60, List.sum2(list1))
    assertEquals(0, List.sum2(emptyList))
  }

  @Test def testFilter(): Unit = {
    assertEquals(list1,filter(list1)((i:Int) => i>0))
    assertEquals(cons(10,cons(30,empty())),filter(list2)(_>0))
    assertEquals(emptyList,filter(emptyList)(_>0))
    assertEquals(of(-20),filter(list2)(_<0))
    assertEquals(of("a"),filter(of("a",""))(_!=""))
  }

  @Test def testDrop(): Unit ={
    println(list1) //Cons(10,Cons(20,Cons(30,Nil())))
    assertEquals(cons(20,cons(30,empty())),drop(list1 ,1)) // Cons (20 , Cons (30 , Nil ()))
    assertEquals(cons(10,cons(30,empty())),drop(list1,2)) //Cons(10,Cons(30,Nil())))
    assertEquals(cons(10,cons(20,empty())),drop(list1,3)) //Cons(10,Cons(20,Nil()))
 //   println(drop(list1,4)) //Cons(10,Cons(20,Cons(30,Nil())))

  }

  @Test def testFlatMap(): Unit ={
    assertEquals(cons(11,cons(21,cons(31,empty()))),flatMap(list1 )(v => cons (v+1, empty()))) // Cons (11 , Cons (21 , Cons (31 , Nil ())))
    assertEquals(cons(11,cons(12,cons(21,cons(22,cons(31,cons(32,empty())))))), flatMap (list1 )(v => cons (v+1, cons (v+2, empty()))))
    // Cons (11 , Cons (12 , Cons (21 , Cons (22 , Cons (31 , Cons (32 , Nil ()))))))
    assertEquals(cons(11,cons(21,cons(31,empty()))),mapAsFlatMap(list1)(v => cons (v+1, empty())))
    assertEquals(cons(false,cons(false,cons(true,empty()))),filterAsFlatMap(list1)(v => cons (v>20, empty())))
  }

  @Test def testMax(): Unit ={
    assertEquals(Some(25),max( cons (10 , cons (25 , cons (20 , empty())))))
    assertEquals(None,max(empty()))
  }

}
