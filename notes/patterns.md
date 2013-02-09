# TREES

Match(selector: Tree, cases: List[CaseDef])
CaseDef(pat: Tree, guard: Tree, body: Tree)
Bind(name: Name, body: Tree)
Star(elem: Tree)
Alternative(trees: List[Tree])
UnApply(fun: Tree, args: List[Tree])


## pattern expressions
  - Apply(TypeTree(), args)              case class constructor pattern
  - UnApply(
      Apply(qual,<unapply-selector>),    for same for unapplySeq. the Apply tree has a symbol to the correct unapply
      List(<pattern, ...)
  - Ident("_")                           to match anything
  - Literals,                            e.g. "abc"
  - Select(...) or Ident(...)            references to stable values
  - Bind(name, <pattern>)
  - Alternative(<pattern>, ...)
  - Typed(_, TypeTree())
  - Star(_)

Guards are a field in CaseDef


# patterns

## Constructor Patterns, Name Bindings, Nested Patterns


abstract class Tree
case class Fork(l: Tree, r: Tree) extends Tree
case class Leaf(x: Int) extends Tree

def foo(t: Tree) = t match {
  case lf @ Leaf(10) => "1"
  case Leaf(x) => "2"
  case Fork(l @ Leaf(_), r) => "3"
  case Fork(l, r) => "4"
}


                Match( // tree.tpe=String
                  "t" // t: Tree, tree.tpe=Tree
                  // 4 cases
                  CaseDef( // tree.tpe=String("1")
                    Bind( // val lf: Leaf, tree.tpe=Leaf
                      "lf"
                      Apply( // tree.tpe=Leaf
                        <tpt> // tree.tpe=(x: Int)Leaf
                        10
                      )
                    )
                    "1"
                  )
                  CaseDef( // tree.tpe=String("2")
                    Apply( // tree.tpe=Leaf
                      <tpt> // tree.tpe=(x: Int)Leaf
                      Bind( // val x: Int, tree.tpe=Int
                        "x"
                        "_" // tree.tpe=Int
                      )
                    )
                    "2"
                  )
                  CaseDef( // tree.tpe=String("3")
                    Apply( // tree.tpe=Fork
                      <tpt> // tree.tpe=(l: Tree, r: Tree)Fork
                      // 2 arguments
                      Bind( // val l: Leaf, tree.tpe=Leaf
                        "l"
                        Apply( // tree.tpe=Leaf
                          <tpt> // tree.tpe=(x: Int)Leaf
                          "_" // tree.tpe=Int
                        )
                      )
                      Bind( // val r: Tree, tree.tpe=Tree
                        "r"
                        "_" // tree.tpe=Tree
                      )
                    )
                    "3"
                  )
                  CaseDef( // tree.tpe=String("4")
                    Apply( // tree.tpe=Fork
                      <tpt> // tree.tpe=(l: Tree, r: Tree)Fork
                      // 2 arguments
                      Bind( // val l: Tree, tree.tpe=Tree
                        "l"
                        "_" // tree.tpe=Tree
                      )
                      Bind( // val r: Tree, tree.tpe=Tree
                        "r"
                        "_" // tree.tpe=Tree
                      )
                    )
                    "4"
                  )
                )
              )
            )



## references to stable identifiers

only stable identifiers: values, objects, lazy vals. objects and lazy vals still have an effect

val Empty = ""
def f(x: String) = x match {
  case Empty => "huh"
  case _ => x
}


                Match( // tree.tpe=String
                  "x" // x: String, tree.tpe=String
                  // 2 cases
                  CaseDef( // tree.tpe=String("huh")
                    "$line31"."$read"."$iw"."$iw"."Empty" // val Empty: String, tree.tpe=String
                    "huh"
                  )
                  CaseDef( // tree.tpe=String
                    "_" // tree.tpe=String
                    "x" // x: String, tree.tpe=String
                  )








## unapply

object Twice {
  def apply(x: Int) = x + x
  def unapply(s: Int) = if (s % 2 == 0) Some(s/2) else None
}

def f(x: Int) = x match {
  case Twice(x) => x
  case _ => -1
}


                Match( // tree.tpe=Int
                  "x" // x: Int, tree.tpe=Int
                  // 2 cases
                  CaseDef( // tree.tpe=Int
                    UnApply( // tree.tpe=Int
                      Apply( // def unapply(s: Int): Option[Int] in object Twice, tree.tpe=Option[Int]
                        "$line27"."$read"."$iw"."$iw"."Twice"."unapply" // def unapply(s: Int): Option[Int] in object Twice, tree.tpe=(s: Int)Option[Int]
                        "<unapply-selector>" // val <unapply-selector>: Int, tree.tpe=Int
                      )
                      List(
                        Bind( // val x: Int, tree.tpe=Int
                          "x"
                          "_" // tree.tpe=Int
                        )
                      )
                    )
                    "x" // val x: Int, tree.tpe=Int
                  )
                  CaseDef( // tree.tpe=Int(-1)
                    "_" // tree.tpe=Int
                    -1
                  )
                )
              )
            )
          )





## unapplySeq

object Split {
  def unapplySeq(s: String) = Some(s.split(" ").toList)
}

"lsdk jf" match {
  case Split(s1, s2) => s1
  case Split(xs @ _*) => xs.mkString(";")
}



                Match( // tree.tpe=String
                  "lsdk jf"
                  // 2 cases
                  CaseDef( // tree.tpe=String
                    UnApply( // tree.tpe=String
                      Apply( // def unapplySeq(s: String): Some[List[String]] in object Split, tree.tpe=Some[List[String]]
                        "$line5"."$read"."$iw"."$iw"."Split"."unapplySeq" // def unapplySeq(s: String): Some[List[String]] in object Split, tree.tpe=(s: String)Some[List[String]]
                        "<unapply-selector>" // val <unapply-selector>: String, tree.tpe=String
                      )
                      List(
                        Bind( // val s1: String, tree.tpe=String
                          "s1"
                          "_" // tree.tpe=String
                        )
                        Bind( // val s2: String, tree.tpe=String
                          "s2"
                          "_" // tree.tpe=String
                        )
                      )
                    )
                    "s1" // val s1: String, tree.tpe=String
                  )
                  CaseDef( // tree.tpe=String
                    UnApply( // tree.tpe=String
                      Apply( // def unapplySeq(s: String): Some[List[String]] in object Split, tree.tpe=Some[List[String]]
                        "$line5"."$read"."$iw"."$iw"."Split"."unapplySeq" // def unapplySeq(s: String): Some[List[String]] in object Split, tree.tpe=(s: String)Some[List[String]]
                        "<unapply-selector>" // val <unapply-selector>: String, tree.tpe=String
                      )
                      List(
                        Bind( // val xs: Seq[String], tree.tpe=String
                          "xs"
                          Star( // tree.tpe=String
                            "_" // tree.tpe=String
                          )
                        )
                      )
                    )
                    Apply( // def mkString(sep: String): String in trait TraversableOnce, tree.tpe=String
                      "xs"."mkString" // def mkString(sep: String): String in trait TraversableOnce, tree.tpe=(sep: String)String
                      ";"
                    )
                  )



## guards

def foo(x: String) = x match {
  case st if (st.length == 10) => 0
  case _ => 1
}

                Match( // tree.tpe=Int
                  "x" // x: String, tree.tpe=String
                  // 2 cases
                  CaseDef( // tree.tpe=Int(0)
                    Bind( // val st: String, tree.tpe=String
                      "st"
                      "_" // tree.tpe=String
                    )
                    Apply( // def ==(x: Int): Boolean in class Int, tree.tpe=Boolean
                      st.length()."$eq$eq" // def ==(x: Int): Boolean in class Int, tree.tpe=(x: Int)Boolean
                      10
                    )
                    0
                  )
                  CaseDef( // tree.tpe=Int(1)
                    "_" // tree.tpe=String
                    1
                  )
                )
              )



## star

occurs as a pattern, e.g. instead of Ident(_) we have Star(Ident(_))

case class C(x: Int, xs: Int*)
def foo(c: C) = c match {
  case C(a) => a
  case C(a,b) => a+b
  case C(a, as @ _*) => a
}


                Match( // tree.tpe=Int
                  "c" // c: C, tree.tpe=C
                  // 3 cases
                  CaseDef( // tree.tpe=Int
                    Apply( // tree.tpe=C
                      <tpt> // tree.tpe=(x: Int, xs: Int*)C
                      Bind( // val a: Int, tree.tpe=Int
                        "a"
                        "_" // tree.tpe=Int
                      )
                    )
                    "a" // val a: Int, tree.tpe=Int
                  )
                  CaseDef( // tree.tpe=Int
                    Apply( // tree.tpe=C
                      <tpt> // tree.tpe=(x: Int, xs: Int*)C
                      // 2 arguments
                      Bind( // val a: Int, tree.tpe=Int
                        "a"
                        "_" // tree.tpe=Int
                      )
                      Bind( // val b: Int, tree.tpe=Int
                        "b"
                        "_" // tree.tpe=Int
                      )
                    )
                    Apply( // def +(x: Int): Int in class Int, tree.tpe=Int
                      "a"."$plus" // def +(x: Int): Int in class Int, tree.tpe=(x: Int)Int
                      "b" // val b: Int, tree.tpe=Int
                    )
                  )
                  CaseDef( // tree.tpe=Int
                    Apply( // tree.tpe=C
                      <tpt> // tree.tpe=(x: Int, xs: Int*)C
                      // 2 arguments
                      Bind( // val a: Int, tree.tpe=Int
                        "a"
                        "_" // tree.tpe=Int
                      )
                      Bind( // val as: Seq[Int], tree.tpe=Int
                        "as"
                        Star( // tree.tpe=Int
                          "_" // tree.tpe=Int
                        )
                      )
                    )
                    "a" // val a: Int, tree.tpe=Int
                  )
                )
              )
            )



## typed patterns


Typed tree appears as a pattern

def f(o: Object): Any = o match {
  case s: String => 1
  case _: List[Int] => -1
  case _: Object => 0
}


                Match( // tree.tpe=Any
                  "o" // o: Object, tree.tpe=Object
                  // 3 cases
                  CaseDef( // tree.tpe=Int(1)
                    Bind( // val s: String, tree.tpe=String
                      "s"
                      Typed( // tree.tpe=String
                        "_" // tree.tpe=String
                        <tpt> // tree.tpe=String
                      )
                    )
                    1
                  )
                  CaseDef( // tree.tpe=Int(-1)
                    Typed( // tree.tpe=List[Int]
                      "_" // tree.tpe=List[Int]
                      <tpt> // tree.tpe=List[Int]
                    )
                    -1
                  )
                  CaseDef( // tree.tpe=Int(0)
                    Typed( // tree.tpe=Object
                      "_" // tree.tpe=Object
                      <tpt> // tree.tpe=Object
                    )
                    0
                  )
                )
              )
            )




## polymorphic case classes

case class C[T](x: T)

def f[T](c: C[T]) = c match {
  case C(te) => te
}


                Match( // tree.tpe=T
                  "c" // c: C[T], tree.tpe=C[T]
                  CaseDef( // tree.tpe=T
                    Apply( // tree.tpe=C[T]
                      <tpt> // tree.tpe=(x: T)C[T]
                      Bind( // val te: T, tree.tpe=T
                        "te"
                        "_" // tree.tpe=T
                      )
                    )
                    "te" // val te: T, tree.tpe=T
                  )
                )
              )



## alternatives

def f(x: Object) = x match {
  case _: String | _: List[Int] => 0
  case _ => 1
}


                Match( // tree.tpe=Int
                  "x" // x: Object, tree.tpe=Object
                  // 2 cases
                  CaseDef( // tree.tpe=Int(0)
                    Alternative( // tree.tpe=Object
                      List(
                        Typed( // tree.tpe=String
                          "_" // tree.tpe=String
                          <tpt> // tree.tpe=String
                        )
                        Typed( // tree.tpe=List[Int]
                          "_" // tree.tpe=List[Int]
                          <tpt> // tree.tpe=List[Int]
                        )
                      )
                    )
                    0
                  )
                  CaseDef( // tree.tpe=Int(1)
                    "_" // tree.tpe=Object
                    1
                  )


## tuple patterns

(1,"2") match {
  case (i,s) => i
}

                  CaseDef( // tree.tpe=Int
                    Apply( // tree.tpe=(Int, String)
                      <tpt> // tree.tpe=(_1: Int, _2: String)(Int, String)
                      // 2 arguments
                      Bind( // val i: Int, tree.tpe=Int
                        "i"
                        "_" // tree.tpe=Int
                      )
                      Bind( // val s: String, tree.tpe=String
                        "s"
                        "_" // tree.tpe=String
                      )
                    )
                    "i" // val i: Int, tree.tpe=Int
                  )
                )





## anonymous functions as case lists

val f: Int => Int = {
  case x if x == 1 => x
  case y => y + 10
}


              ValDef( // private[this] val f: Int => Int
                private <local> <triedcooking>
                "f "
                <tpt> // tree.tpe=Int => Int
                Function( // val $anonfun: <notype>, tree.tpe=Int => Int
                  ValDef( // x0$1: Int
                    <param> <synthetic> <triedcooking>
                    "x0$1"
                    <tpt> // tree.tpe=Int
                    <empty>
                  )
                  Match( // tree.tpe=Int
                    "x0$1" // x0$1: Int, tree.tpe=Int
                    // 2 cases
                    CaseDef( // tree.tpe=Int
                      Bind( // val x: Int, tree.tpe=Int
                        "x"
                        "_" // tree.tpe=Int
                      )
                      Apply( // def ==(x: Int): Boolean in class Int, tree.tpe=Boolean
                        "x"."$eq$eq" // def ==(x: Int): Boolean in class Int, tree.tpe=(x: Int)Boolean
                        1
                      )
                      "x" // val x: Int, tree.tpe=Int
                    )
                    CaseDef( // tree.tpe=Int
                      Bind( // val y: Int, tree.tpe=Int
                        "y"
                        "_" // tree.tpe=Int
                      )
                      Apply( // def +(x: Int): Int in class Int, tree.tpe=Int
                        "y"."$plus" // def +(x: Int): Int in class Int, tree.tpe=(x: Int)Int
                        10
                      )
                    )
                  )
                )







## val patterns:

case class C(a: Int, b: Int)
val C(u,v) = C(10,12)



              ValDef( // private[this] val x$1: (Int, Int)
                private <local> <synthetic> <triedcooking>
                "x$1"
                <tpt> // tree.tpe=(Int, Int)
                Match( // tree.tpe=(Int, Int)
                  Typed( // tree.tpe=C @unchecked
                    Apply( // case def apply(a: Int,b: Int): C in object C, tree.tpe=C
                      "$line25"."$read"."$iw"."$iw"."C"."apply" // case def apply(a: Int,b: Int): C in object C, tree.tpe=(a: Int, b: Int)C
                      // 2 arguments
                      10
                      12
                    )
                    <tpt> // tree.tpe=C @unchecked
                  )
                  CaseDef( // tree.tpe=(Int, Int)
                    Apply( // tree.tpe=C
                      <tpt> // tree.tpe=(a: Int, b: Int)C
                      // 2 arguments
                      Bind( // val u: Int, tree.tpe=Int
                        "u"
                        "_" // tree.tpe=Int
                      )
                      Bind( // val v: Int, tree.tpe=Int
                        "v"
                        "_" // tree.tpe=Int
                      )
                    )
                    Apply( // case def apply[T1, T2](_1: T1,_2: T2): (T1, T2) in object Tuple2, tree.tpe=(Int, Int)
                      TypeApply( // case def apply[T1, T2](_1: T1,_2: T2): (T1, T2) in object Tuple2, tree.tpe=(_1: Int, _2: Int)(Int, Int)
                        "scala"."Tuple2"."apply" // case def apply[T1, T2](_1: T1,_2: T2): (T1, T2) in object Tuple2, tree.tpe=[T1, T2](_1: T1, _2: T2)(T1, T2)
                        // 2 type arguments
                        <tpt> // tree.tpe=Int
                        <tpt> // tree.tpe=Int
                      )
                      // 2 arguments
                      "u" // val u: Int, tree.tpe=Int
                      "v" // val v: Int, tree.tpe=Int
                    )
                  )
                )
              )
              ValDef( // private[this] val u: Int
                private <local> <triedcooking>
                "u "
                <tpt> // tree.tpe=Int
                $iw.this."x$1"."_1" // val _1: T1 in class Tuple2, tree.tpe=Int
              )
              ValDef( // private[this] val v: Int
                private <local> <triedcooking>
                "v "
                <tpt> // tree.tpe=Int
                $iw.this."x$1"."_2" // val _2: T2 in class Tuple2, tree.tpe=Int
              )
            )
          )
        )
      )






## not pattern matching. typed sequence, as function argument

def f(xs: Int*) = 0
f(List(1): _*)



                Apply( // def f(xs: Int*): Int, tree.tpe=Int
                  "$line4"."$read"."$iw"."$iw"."f" // def f(xs: Int*): Int, tree.tpe=(xs: Int*)Int
                  Typed( // tree.tpe=Int
                    Apply( // override def apply[A](xs: A*): List[A] in object List, tree.tpe=List[Int]
                      TypeApply( // override def apply[A](xs: A*): List[A] in object List, tree.tpe=(xs: Int*)List[Int]
                        immutable.this."List"."apply" // override def apply[A](xs: A*): List[A] in object List, tree.tpe=[A](xs: A*)List[A]
                        <tpt> // tree.tpe=Int
                      )
                      1
                    )
                    "_*" // tree.tpe=Int
                  )
                )


