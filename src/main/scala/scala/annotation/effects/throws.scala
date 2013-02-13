package scala.annotation.effects

class throws[E <: Throwable] extends Effect

trait |[T <: Throwable, U <: Throwable] extends Throwable
