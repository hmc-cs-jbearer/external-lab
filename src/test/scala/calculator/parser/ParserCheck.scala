package calculator.parser

import scala.language.implicitConversions
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties
import calculator.ir._

object CalcParseSpec extends Properties("Parser") {

    // some syntactic sugar for expressing parser tests
    implicit class ParseResultChecker(input: String) {
      def ~>(output: Expr) = {
        val result = CalcParser(input)
        result.successful && result.get == output
      }
    }

    property("numbers") = forAll { (n: Int) ⇒
      s"$n" ~> Num(n)
    }

    // Addition and subtraction

    property("addition") = forAll { (n1: Int, n2: Int) ⇒
      s"$n1 + $n2" ~> (Plus(Num(n1), Num(n2)))
    }

    property("subtraction") = forAll { (n1: Int, n2: Int) =>
      s"$n1 - $n2" ~> (Minus(Num(n1), Num(n2)))
    }

    property("subtraction associativity") =
    forAll { (n1: Int, n2: Int, n3: Int) =>
      s"$n1 - $n2 - $n3" ~> (Minus(Minus(Num(n1), Num(n2)), Num(n3)))
    }

    property("addition/subtraction precedence") =
    forAll { (n1: Int, n2: Int, n3: Int) =>
      s"$n1 + $n2 - $n3" ~> (Minus(Plus(Num(n1), Num(n2)), Num(n3))) &&
      s"$n1 - $n2 + $n3" ~> (Plus(Minus(Num(n1), Num(n2)), Num(n3)))
    }

    // Multiplication and division

    property("multiplication") = forAll { (n1: Int, n2: Int) =>
      s"$n1 * $n2" ~> (Times(Num(n1), Num(n2)))
    }

    property("multiplication/addition precedence") =
    forAll { (n1: Int, n2: Int, n3: Int) =>
      s"$n1 + $n2 * $n3" ~> (Plus(Num(n1), Times(Num(n2), Num(n3)))) &&
      s"$n1 * $n2 + $n3" ~> (Plus(Times(Num(n1), Num(n2)), Num(n3)))
    }

    property("multiplication/subtraction precedence") =
    forAll { (n1: Int, n2: Int, n3: Int) =>
      s"$n1 - $n2 * $n3" ~> (Minus(Num(n1), Times(Num(n2), Num(n3)))) &&
      s"$n1 * $n2 - $n3" ~> (Minus(Times(Num(n1), Num(n2)), Num(n3)))
    }

    property("division") = forAll { (n1: Int, n2: Int) =>
      s"$n1 / $n2" ~> Divide(Num(n1), Num(n2))
    }

    property("division associativity") =
    forAll { (n1: Int, n2: Int, n3: Int) =>
      s"$n1 / $n2 / $n3" ~> (Divide(Divide(Num(n1), Num(n2)), Num(n3)))
    }

    property("multiplication/division precedence") =
    forAll { (n1: Int, n2: Int, n3: Int) =>
      s"$n1 * $n2 / $n3" ~> (Divide(Times(Num(n1), Num(n2)), Num(n3))) &&
      s"$n1 / $n2 * $n3" ~> (Times(Divide(Num(n1), Num(n2)), Num(n3)))
    }

    property("division/addition precedence") =
    forAll { (n1: Int, n2: Int, n3: Int) =>
      s"$n1 + $n2 / $n3" ~> (Plus(Num(n1), Divide(Num(n2), Num(n3)))) &&
      s"$n1 / $n2 + $n3" ~> (Plus(Divide(Num(n1), Num(n2)), Num(n3)))
    }

    property("division/subtraction precedence") =
    forAll { (n1: Int, n2: Int, n3: Int) =>
      s"$n1 - $n2 / $n3" ~> (Minus(Num(n1), Divide(Num(n2), Num(n3)))) &&
      s"$n1 / $n2 - $n3" ~> (Minus(Divide(Num(n1), Num(n2)), Num(n3)))
    }

    property("parenthetical precedence") =
    forAll { (n1: Int, n2: Int, n3: Int) =>
      s"$n1 * ($n2 + $n3)" ~> (Times(Num(n1), Plus(Num(n2), Num(n3))))
    }

}
