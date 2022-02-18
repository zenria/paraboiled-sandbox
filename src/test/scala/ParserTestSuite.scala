import ParserTestSuite.parse
import org.scalatest.funsuite.AnyFunSuite

class ParserTestSuite extends AnyFunSuite {
  val foo = Text("foo")
  val bar = Text("bar")
  val baz = Text("baz")

  test("word") {
    assert(parse("foo") == Text("foo"))
    assert(parse("(foo)") == Text("foo"))
    assert(parse(" foo") == Text("foo"))
    assert(parse(" foo ") == Text("foo"))
    assert(parse("foo ") == Text("foo"))
    assert(parse("  foo") == Text("foo"))
    assert(parse("  foo  ") == Text("foo"))
    assert(parse("foo  ") == Text("foo"))
  }

  test("or") {
    assert(parse("foo OR bar") == Or(Seq(foo, bar)))
    assert(parse("foo OR bar OR baz") == Or(Seq(foo, bar, baz)))
    assert(parse("(foo OR bar)") == Or(Seq(foo, bar)))
    assert(parse("foo OR (bar OR baz)") == Or(Seq(foo, Or(Seq(bar, baz)))))
    assert(parse("(foo OR bar) OR baz") == Or(Seq(Or(Seq(foo, bar)), baz)))
  }

  test("and") {
    assert(parse("foo AND bar") == And(Seq(foo, bar)))
    assert(parse("foo AND bar AND baz") == And(Seq(foo, bar, baz)))

    assert(parse("foo OR bar AND baz") == Or(Seq(foo, And(Seq(bar, baz)))))
    assert(parse("foo AND bar OR baz") == Or(Seq(And(Seq(foo, bar)), baz)))
    assert(parse("(foo OR bar) AND baz") == And(Seq(Or(Seq(foo, bar)), baz)))
    assert(parse("foo AND (bar OR baz)") == And(Seq(foo, Or(Seq(bar, baz)))))
  }
}

object ParserTestSuite {
  def parse(s: String): Expr = {
    new ExprParser().parse(s)
  }
}
