package u03

import u02.Lists.List.{cons, empty, of}
import u02.Lists.List._
import u02.Lists
import u02.Optionals.Option


object Fold {
  import Lists.List

  def foldLeft[A](l: List[A])(initial:A)(f: (A,A) => A): A = {
    def _foldLeft[A](list: List[A])(acc: A)(f: (A,A) => A): A = list match {
      case Cons(h, t) => _foldLeft(t)(f(acc,h))(f)
      case Nil() => acc
    }
    _foldLeft(l)(initial)(f)
  }

  def foldRight[A](l: List[A])(initial:A)(f: (A,A) => A): A = {
    def _foldRight[A](list: List[A])(acc: A)(f: (A,A) => A): A = list match {
      case Cons(h, t) => f(h,_foldRight(t)(acc)(f))
      case Nil() => acc
    }
    _foldRight(l)(initial)(f)
  }


}
