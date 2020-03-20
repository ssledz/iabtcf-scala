package io.github.ssledz.fp

trait Show[A] {
  def show(a: A): String
}

object Show {
  def apply[A: Show]: Show[A] = implicitly[Show[A]]

  implicit def optionShowInstance[A: Show]: Show[Option[A]] = new Show[Option[A]] {
    def show(a: Option[A]): String = a match {
      case Some(value) => "Some(" + implicitly[Show[A]].show(value) + ")"
      case None => "None"
    }
  }

  implicit class ShowSyntax[A](val a: A) extends AnyVal {
    def show(implicit S: Show[A]): String = S.show(a)
  }

}

