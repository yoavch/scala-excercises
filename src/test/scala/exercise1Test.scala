import org.scalatest.FunSuite

class exercise1Test extends FunSuite {

  def sameElements(ls1:List[Any], ls2: List[Any]) = {
    // compare elements in one list same as elements in another lists
    ls1.size == ls2.size && ls1.intersect(ls2).size == ls1.size
  }

  test("sameElements") {
    val ls1 = List(1, 9, 3)
    val ls2 = List(1, 3, 9)
    assert(sameElements(ls1, ls2))
    val ls3 = List(1, 9, 3, 4)
    assert(!sameElements(ls1, ls3))

  }

  test("testMax3multipliers zero elements") {
    val ls1 = List()
    assert(exercise1.max3multipliers(ls1).sameElements(List()) )
  }

  test("testMax3multipliers less than 3 elements") {
    val ls1 = List(1, -1)
    assert(exercise1.max3multipliers(ls1).sameElements(List()) )
  }

  test("testMax3multipliers exactly 3 elements") {
    val ls1 = List(1, -1, 2)
    assert(exercise1.max3multipliers(ls1).sameElements(List(1, -1, 2)) )
  }

  test("testMax3multipliers 1 postivie other negatives ") {
    val ls1 = List(-1, -9, 1, -1, -5)
    assert(exercise1.max3multipliers(ls1).sameElements(List(-9, 1, -5)) )
  }

  test("testMax3multipliers 3 postivie other negatives ") {
    val ls1 = List(-1, -9, 1, -1, -5, 12, 7, 8, 24)
    assert(exercise1.max3multipliers(ls1).sameElements(List(12, 8, 24)) )
  }

  test("testMax3multipliers 2 postivie other negatives ") {
    val ls1 = List(-1, -9, 1, -1, -5, -12, 2, -8, -24)
    assert(exercise1.max3multipliers(ls1).sameElements(List(-12, 2, -24)) )
  }

}
