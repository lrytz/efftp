import annotation.effects._

class C1 {
  trait S[A]

  trait Buf[A] { self =>
    def readOnly: S[A] = ???
  }

  trait BufPro[A] extends Buf[A] {
    override def readOnly: S[A] = self.readOnly
  }
}

class C2 {
  trait S[A]

  trait BufferLike[A] { self =>
    def readOnly: S[A] = ???
  }

  trait Buffer[A] extends BufferLike[A]

  trait BufferProxy[A] extends Buffer[A] {

    override def readOnly: S[A] = self.readOnly
  }
}
