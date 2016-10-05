package calculator.semantics

import scala.language.implicitConversions
import org.scalacheck._
import org.scalacheck.Prop.{forAll,BooleanOperators,throws}
import Gen._
import Arbitrary.arbitrary
import calculator.ir._

object CalcInterpreterSpec extends Properties("Interpreter") {

    // some syntactic sugar for expressing interpreter tests
    implicit class TreeChecker(input: Expr) {
      def ~>(output: Int) = {
        val result = eval(input)
        result == output
      }

      def ~/~>[T <: Throwable](c: Class[T]) = {
        throws(c){eval(input)}
      }
    }

    property("numbers") = forAll { (n: Int) ⇒
      Num(n) ~> n
    }

    property("addition") = forAll { (n1: Int, n2: Int) ⇒
      (Plus(Num(n1), Num(n2))) ~> (n1 + n2)
    }

    property("subtraction") = forAll { (n1: Int, n2: Int) =>
      (Minus(Num(n1), Num(n2))) ~> (n1 - n2)
    }

    property("multiplication") = forAll { (n1: Int, n2: Int) =>
      (Times(Num(n1), Num(n2))) ~> (n1 * n2)
    }

    property("division") = forAll { (n1: Int, n2: Int) =>
      n2 == 0 || (Divide(Num(n1), Num(n2))) ~> (n1 / n2)
    }

}