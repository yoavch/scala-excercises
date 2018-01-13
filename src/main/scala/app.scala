import scala.io.Source

object exercise1 extends App {

  def readList(): List[Int] = {
    val fileName = "data.txt"
    val resList = for (line <- Source.fromFile(fileName).getLines();
                       elem <- line.split("\\s+"))
      yield elem.toInt
    resList.toList
  }

  def printList(ls: List[Int]) = {
    for (elem <- ls)
      print(s"$elem ")
    println
  }

  case class PosNeg (pos: Int, neg: Int)
  def countPositiveNegatives(ints: List[Int]) = {
    val numPos = ints.count(_ >= 0)
    val numNeg = ints.count(_ < 0)
    PosNeg(numPos, numNeg)
  }

  def max3multipliers(ls: List[Int]): List[Int]  = {
    val cntPosNeg: PosNeg = countPositiveNegatives(ls)

    val top3l: List[Int] = cntPosNeg match {
      case x1 if x1.pos + x1.neg < 3 => List()
      case x2 if x2.pos + x2.neg == 3 => ls
      case x3 if x3.pos >= 3 => max3Pos(ls)
      case x4 if x4.pos == 2 => max2PosNeg(ls)
      case x5 if x5.pos == 1 => max1Pos(ls)
      case x5 if x5.pos == 0 => maxNeg(ls)
    }
    top3l
  }

  def topKElems(ls: List[Int], k: Int, sign: Int = 1, isAbs: Boolean = false): List[Int] = {
    // returns the top k elements from l, which has the given sigh (1: positiv, -1: negative)
    // if isAbs is true, returns the largest ABS elements (sign = -1 and isAbs = false yields the smallest negative numbers
    def maxK(ls: List[Int], k: Int, klist: List[Int] = List()): List[Int] = {
      // given ls, k returns the top (largest or smallest) element in ls which is not in k, concatenated to k
      // comparer determines whether we are after the max or min

      val signcond = (e: Int) => sign < 0 && e < 0 || sign > 0 && e >= 0

      def isGT(a: Int, b: Int) = if (isAbs) Math.abs(a) > Math.abs(b) else a > b

      if (k == 0)
        klist
      else {
          val maxnum: Int  =
            ((ls.filter(signcond).foldLeft(if (sign > 0 || isAbs) 0 else Int.MinValue))
            ((maxn, elem) => if (!klist.contains(elem) && isGT(elem, maxn)) elem else maxn))
          maxK(ls, k - 1, maxnum :: klist)
        }
      }
    maxK(ls, k, List())
  }


  def max3Pos(ls: List[Int]) = {
    // returns the larges 3 postivie numbers
    topKElems(ls, 3)
  }


  def max2PosNeg(ls: List[Int]) = {
    // there are 2 positive and 2 or more negaitves
    // take the largest of the positive and the smallest 2 negatives
    topKElems(ls, 2, -1, isAbs = true) ::: topKElems(ls, 1)
  }

  def max1Pos(ls: List[Int]) = {
    // there is 1 positive number. Take the 2 smallest negative numbers and multiply by the positive number
    topKElems(ls, 1) ::: topKElems(ls, 2, -1, true)
  }

  def maxNeg(ls: List[Int]) = {
    // all elements are negative. Take the pair of 3 largest negative numbers and multiply them by each
    topKElems(ls, 3, -1)
  }

  val ls: List[Int] = readList()
  println("Input list")
  printList(ls)

  val l3 = max3multipliers(ls)
  println("Top 3 multipliers")
  printList(l3)
}
