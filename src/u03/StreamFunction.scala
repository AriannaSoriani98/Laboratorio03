package u03

import u03.Streams.Stream.{Cons, Empty, cons, iterate}
import u03.Streams.Stream._

object StreamFunction {
  import u03.Streams.Stream


  def drop[A](stream: Stream[A])(n:Int): Stream[A] ={
    def _drop[A](s: Stream[A])(n_iter:Int): Stream[A] = s match{
    case Cons(_, tail) if(n_iter <= n) => _drop(tail())(n_iter+1)
    case Cons(head,tail) => cons(head(), tail())
    case _ => Empty()
  }
    _drop(stream)(1)
  }

  def constant[A](value:A):Stream[A]= cons(value,constant(value))


  val fib:Stream[Int] ={
    def _fib(a:Int, b:Int):Stream[Int] =  cons(a+b,_fib(b,a + b))
    cons(0,cons(1,_fib(0,1)))
  }
}