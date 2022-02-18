import org.parboiled.errors.{ErrorUtils, ParsingException}
import org.parboiled.scala._

sealed abstract class Expr
case class Text(inner: String)     extends Expr
case class And(clauses: Seq[Expr]) extends Expr
case class Or(clauses: Seq[Expr])  extends Expr

/**
  * Let's try to parse expression having the form:
  *
  * foo AND bar
  * foo OR bar AND baz
  * (foo OR bar) AND baz AND coucou
  * ... etc
  *
  */
class ExprParser extends Parser {

  def InputLine: Rule1[Expr] = rule {
    WhiteSpaces0 ~ Expression ~ EOI
  }

  def Expression: Rule1[Expr] =
    AndClause ~ zeroOrMore("OR" ~ WhiteSpaces1 ~ AndClause) ~~> ((clause: Expr,
                                                                  subsequentOrClauses: List[Expr]) =>
                                                                   if (subsequentOrClauses.isEmpty) {
                                                                     clause
                                                                   } else {
                                                                     Or(Seq(clause).concat(subsequentOrClauses))
                                                                   })

  def AndClause: Rule1[Expr] =
    Term ~ zeroOrMore("AND" ~ WhiteSpaces1 ~ Term) ~~> ((clause: Expr,
                                                         subsequentAndClauses: List[Expr]) =>
                                                          if (subsequentAndClauses.isEmpty) {
                                                            clause
                                                          } else {
                                                            And(Seq(clause).concat(subsequentAndClauses))
                                                          })

  def Term: Rule1[Expr] = rule {
    Word | Parens | Expression
  }

  def Parens: Rule1[Expr] = rule {
    "(" ~ WhiteSpaces0 ~ Expression ~ ")" ~ WhiteSpaces0
  }

  def Word: Rule1[Text] = rule {
    oneOrMore(WordChar) ~> Text ~ WhiteSpaces0
  }

  def WordChar: Rule0 = rule {
    "a" - "z" | "A" - "Z"
  }

  def WhiteSpaces1: Rule0 = rule {
    oneOrMore(anyOf(" \n\r\t\f"))
  }

  def WhiteSpaces0: Rule0 = rule {
    zeroOrMore(anyOf(" \n\r\t\f"))
  }

  def parse(expression: String): Expr = {
    val parsingResult = ReportingParseRunner(InputLine).run(expression)
    parsingResult.result match {
      case Some(i) => i
      case None =>
        throw new ParsingException(
          "Invalid search expression:\n" +
            ErrorUtils.printParseErrors(parsingResult))
    }
  }
}
