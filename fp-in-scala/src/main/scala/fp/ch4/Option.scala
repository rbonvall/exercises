package fp.ch4

sealed trait Option[+A] {
  def map[B](f: A ⇒ B): Option[B] = this match {
    case Some(x) ⇒ Some(f(x))
    case None    ⇒ None
  }
  def orElse[B >: A](ob: ⇒ Option[B]): Option[B] = this match {
    case Some(x) ⇒ this
    case None    ⇒ ob
  }

  /* The following combinators are defined both with
   * and without resorting to pattern matching. */
  def flatMapPM[B](f: A ⇒ Option[B]): Option[B] = this match {
    case Some(x) ⇒ f(x)
    case None    ⇒ None
  }
  def flatMap[B](f: A ⇒ Option[B]): Option[B] =
    this.map(f).orElse(Some(None)).asInstanceOf[Some[Option[B]]].get

  def getOrElsePM[B >: A](default: ⇒ B): B = this match {
    case Some(x) ⇒ x
    case None    ⇒ default
  }
  def getOrElse[B >: A](default: ⇒ B): B =
    this.orElse(Some(default)).asInstanceOf[Some[B]].get

  def filterPM(f: A ⇒ Boolean): Option[A] = this match {
    case Some(x) if f(x) ⇒ this
    case _               ⇒ None
  }
  def filter(f: A ⇒ Boolean): Option[A] =
    this.flatMap { x ⇒ if (f(x)) Some(x) else None }

}

case class  Some[+A](get: A) extends Option[A]
case object None             extends Option[Nothing]

object Option {
  def lift[A, B](f: A ⇒ B): Option[A] ⇒ Option[B] = _ map f
}
