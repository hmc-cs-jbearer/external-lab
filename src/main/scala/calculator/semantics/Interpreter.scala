package calculator

import calculator.ir._

package object semantics {
  def eval(expr: Expr): Int = expr match {
    case Num(i) ⇒ i
    case Plus(left, right) ⇒ eval(left) + eval(right)
    case Minus(left, right) => eval(left) - eval(right)
    case Times(left, right) => eval(left) * eval(right)
    case Divide(left, right) => eval(left) / eval(right)
  }
}