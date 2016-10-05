package calculator.parser

import scala.util.parsing.combinator._
import calculator.ir._

/**
 * -----------
 * Grammar
 * -----------
 *
 *                   n ∈ 𝒵
 *
 *       e ∈ Expr ::= e + t | e - t | t
 *       t ∈ Term ::= t * f | t / f | f
 *       f ∈ Fact ::= n | ( e )
 *
 */

object CalcParser extends JavaTokenParsers with PackratParsers{

    // parsing interface
    def apply(s: String): ParseResult[Expr] = parseAll(expr, s)

    // expressions
    lazy val expr: PackratParser[Expr] =
      (   expr~"+"~fact ^^ {case l~"+"~r ⇒ Plus(l, r)}
        | fact )

    // factors
    lazy val fact: PackratParser[Expr] =
      number

    // numbers
    def number: Parser[Num] = wholeNumber ^^ {s ⇒ Num(s.toInt)}

 }
