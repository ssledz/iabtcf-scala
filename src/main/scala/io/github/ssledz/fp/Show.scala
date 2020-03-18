package io.github.ssledz.fp

trait Show[A] {
  def show(a: A): String
}

object Show {
  def apply[A: Show]: Show[A] = implicitly[Show[A]]

  implicit class ShowSyntax[A](val a: A) extends AnyVal {
    def show(implicit S: Show[A]): String = S.show(a)
  }

}

