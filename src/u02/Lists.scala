package u02

import scala.annotation.tailrec

object Lists {

  // ADT List
  trait List[A]

  object List {

    // ADT constructors
    case class Cons[A](head: A, tail: List[A]) extends List[A]

    case class Nil[A]() extends List[A]

    def empty[A](): List[A] = Nil()

    def of[A](a: A): List[A] = Cons(a, Nil())

    def of[A](a1: A, a2: A): List[A] = Cons(a1, Cons(a2, Nil()))

    def cons[A](h: A, t: List[A]): List[A] = Cons(h, t)

    def sum2(list: List[Int]): Int = {
      @tailrec def _sum(l: List[Int], s: Int): Int = l match {
        case Cons(h, t) => _sum(t, s + h)
        case Nil() => s
      }

      _sum(list, 0)
    }

    def sum(list: List[Int]): Int = list match {
      case Cons(h, t) => h + sum(t)
      case Nil() => 0
    }

    def filter[A](list: List[A])(pred: A => Boolean): List[A] = list match {
      case Cons(h, t) if (pred(h)) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()
    }

    def drop[A](list: List[A], n: Int): List[A] = {
      def _drop[A](l: List[A], index: Int): List[A] = l match {
        case Cons(h, t) if (n != index) => Cons(h, _drop(t, index + 1))
        case Cons(_, t) =>  _drop(t, index + 1)
        case Nil() => Nil()
      }
      _drop(list, 1)
    }

    def append[A](l1: List[A], l2: List[A]): List[A] = (l1, l2) match {
      case (Cons(h, t), l2) => Cons(h, append(t, l2))
      case _ => l2
    }

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match{
        case Cons(h, t) => append(f(h),flatMap(t)(f))
        case Nil() => Nil()

    }

    def max(l: List[Int]): Option[Int]= {
      def _max(l:List[Int], n_max:Int): Option[Int]= l match{
        case Cons(h,t) if (h>n_max) => _max(t,n_max=h)
        case Cons(_,Nil()) => Some(n_max)
        case Nil() => None
      }
      _max(l,0)
    }

    def mapAsFlatMap[A,B](l: List[A])(mapper: A=> List[B]): List[B] = l match {
      case Cons(h, t) => flatMap(l)(mapper)
      case Nil() => Nil()
    }

    def filterAsFlatMap[A,B](l1: List[A])(pred: A=>List[B]): List[B] = l1 match {
      case Cons(_,_) => flatMap(l1)(pred)
      case Nil() => Nil()
    }



  }
}
