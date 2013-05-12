import annotation.effects._

class C {
  def no: Unit @noIo = ()
  def yo: Unit @io   = ()

  // neg ascription
  def f2: Unit @noIo = {
    (no: @io)
  }
  
  // neg ascription
  def f3: Unit @noIo = {
    (yo: @noIo)
  }
}
