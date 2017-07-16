package calculator

import java.util.Optional

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map (ne => (ne._1, Signal(evaluate(ne._2(), namedExpressions, Set(ne._1)))))
  }

  def evaluate(e: Expr, references: Map[String, Signal[Expr]], cyclic: Set[String]): Double = {
    e match {
      case Literal(v) => v
      case Ref(name) => {
        if (cyclic.contains(name)) Double.NaN
        else if (references.contains(name)) {
          evaluate(references(name)(), references, cyclic ++ Set(name))
        }
        else Double.NaN
      }
      case Plus(a, b) => evaluate(a, references, cyclic) + evaluate(b, references, cyclic)
      case Minus(a, b) => evaluate(a, references, cyclic) - evaluate(b, references, cyclic)
      case Times(a, b) => evaluate(a, references, cyclic) * evaluate(b, references, cyclic)
      case Divide(a, b) => evaluate(a, references, cyclic) / evaluate(b, references, cyclic)
    }
  }


  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
