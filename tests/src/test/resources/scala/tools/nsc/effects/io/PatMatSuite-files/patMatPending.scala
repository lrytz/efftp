import annotation.effects._

trait IterableSplitter[+T]
class A[T] {

  @transient private var array: Array[Any] = ???

  trait ScanTree[U >: T]

  case class ScanNode[U >: T](left: ScanTree[U], right: ScanTree[U]) extends ScanTree[U]
  

  case class ScanLeaf[U >: T](pit: IterableSplitter[U], op: (U, U) => U, from: Int, len: Int, var prev: Option[ScanLeaf[U]], var acc: U)
    extends ScanTree[U] {
  }

  class ScanToArray[U >: T](tree: ScanTree[U], z: U, op: (U, U) => U, targetarr: Array[Any]) {
    private def iterate(tree: ScanTree[U]): Unit = tree match {
      case ScanNode(left, right) =>
        iterate(left)
        iterate(right)
      case ScanLeaf(_, _, from, len, Some(prev), _) =>
        scanLeaf(array, targetarr, from, len, prev.acc)
      case ScanLeaf(_, _, from, len, None, _) =>
        scanLeaf(array, targetarr, from, len, z)
    }
    private def scanLeaf(srcarr: Array[Any], targetarr: Array[Any], from: Int, len: Int, startval: U) {
    }
  }
}
