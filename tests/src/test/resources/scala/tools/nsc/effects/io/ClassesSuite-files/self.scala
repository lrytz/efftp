import annotation.effects._

class C1 {
  trait S[A]

  trait BufferLike[A] { self =>
    def readOnly: S[A] = ???
  }

  trait Buffer[A] extends BufferLike[A]

  trait BufferProxy[A] extends Buffer[A] {
    def self: Buffer[A]
    override def readOnly: S[A] = self.readOnly
  }
}

class C2 {
  trait Seq[+A] extends SeqLike[A]

  trait SeqLike[+A] { self =>
  }

  trait BufferLike[A] extends SeqLike[A] { self =>
    def readOnly: Seq[A] = ???
  }

  trait Buffer[A] extends Seq[A] with BufferLike[A]

  trait BufferProxy[A] extends Buffer[A] {
    def self: Buffer[A]

    override def readOnly = self.readOnly
  }
}

class C3 {
  trait Seq[+A] extends SeqLike[A]

  trait SeqLike[+A] { self =>
    def toSeq: Seq[A] = ???
  }

  trait BufferLike[A] extends SeqLike[A] { // self =>
    def readOnly: Seq[A] = ???
  }

  trait Buffer[A] extends Seq[A] with BufferLike[A]

  trait BufferProxy[A] extends Buffer[A] {
    def self: Buffer[A]

    override def readOnly = self.readOnly
  }
}
