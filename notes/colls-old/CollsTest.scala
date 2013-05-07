package collsOld

object CollsTest {
  def main(args: Array[String]) {
    val l: Lst[Int] = new cns(1, new cns(2, new cns(3, nl))) // Lst(1,2,3) @TODO
    val m: Lst[Int] = l filter (x => x > 1)
    println(m)

    println(l map (x => x + 1))

    var s = new HSt[String]()
    s += "ldskjf"
    s += "lsdkf"
    s = s map (x => x + "0-00")
    for (i <- s) print(i +", ")
    println()

    for (i <- s filter (x => x contains "j")) print(i +", ")
    println()

    var bs: BtSt = new BtStImpl()
    bs += 1
    bs += 2
    bs = bs map (x => x + 1)
    for (i <- bs) print(i +", ")
    println()

    for (i <- bs filter (x => x > 2)) print(i +", ")
    println()

    //bs = bs map (x => ":"+ x)
    val bss: St[String] = bs map (x => ":"+ x)
    for (i <- bss) print(i + ", ")
    println()

    for (i <- bss filter (x => x contains "2")) print(i +", ")
    println()
  }
}
