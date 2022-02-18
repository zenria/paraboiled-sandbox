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
    assert(parse("foo OR bar") == Or(foo, bar))
    assert(parse("foo OR bar OR baz") == Or(Or(foo, bar), baz))
    assert(parse("(foo OR bar)") == Or(foo, bar))
    assert(parse("foo OR (bar OR baz)") == Or(foo, Or(bar, baz)))
    assert(parse("(foo OR bar) OR baz") == Or(Or(foo, bar), baz))
  }

  test("and") {
    assert(parse("foo AND bar") == And(foo, bar))
    assert(parse("foo AND bar AND baz") == And(And(foo, bar), baz))

    assert(parse("foo OR bar AND baz") == Or(foo, And(bar, baz)))
    assert(parse("foo AND bar OR baz") == Or(And(foo, bar), baz))
    assert(parse("(foo OR bar) AND baz") == And(Or(foo, bar), baz))
    assert(parse("foo AND (bar OR baz)") == And(foo, Or(bar, baz)))
  }
}

object ParserTestSuite {
  def parse(s: String): Expr = {
    new ExprParser().parse(s)
  }
}
