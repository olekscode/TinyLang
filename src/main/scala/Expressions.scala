trait Expr {
  def isReducible = false

  def name: String =
    throw new Error("This type of expression does not have a name")

  def numValue: Int =
    throw new Error("This type of expression does not have a numeric value")

  def boolValue: Boolean =
    throw new Error("This type of expression does not have a boolean value")

  def arguments: Seq[Expr] =
    throw new Error("This type of does not accept arguments")

  def apply(args: Expr*): Expr =
    throw new Error("This type of expression is not applicable")

  def exprType: String = this.getClass.getName
}

abstract class SelfEvaluatingExpr extends Expr

case class Number(n: Int) extends SelfEvaluatingExpr {
  override def numValue: Int = n

  override def toString: String =
    numValue.toString()
}

case class Bool(b: Boolean) extends SelfEvaluatingExpr {
  override def boolValue: Boolean = b

  override def toString: String =
    boolValue.toString()
}

case class Var(n: String) extends Expr {
  override def isReducible = true
  override def name = n
}

abstract class Procedure(args: Expr*) extends Expr {
  override def isReducible = true
  override def name = exprType
  override def arguments = args

  override def toString: String = {
    val argsStr = arguments.mkString(" ")
    s"($name $argsStr)"
  }
}

abstract class PrimitiveProcedure(args: Expr*) extends Procedure(args:_*) {
  def applyInnerProcedure(a: Expr, b: Expr): Expr

  override def apply(args: Expr*): Expr = {
    if (args.isEmpty) throw new Error("List of arguments is empty")
    else {
      def applyIter(x: Expr, arr: Seq[Expr]): Expr = {
        if (arr.isEmpty) x
        else {
          val res = applyInnerProcedure(x, arr.head)
          applyIter(res, arr.tail)
        }
      }
      applyIter(args.head, args.tail)
    }
  }
}

abstract class NumericPrimitiveProcedure(args: Expr*) extends PrimitiveProcedure(args:_*) {
  def applyInnerProcedure(a: Expr, b: Expr): Expr = {
    val aInt = a.numValue
    val bInt = b.numValue
    val c = innerProcedure(aInt, bInt)
    Number(c.asInstanceOf[Int])
  }

  def innerProcedure(a: Int, b: Int): Int
}

abstract class BooleanPrimitiveProcedure(args: Expr*) extends PrimitiveProcedure(args:_*) {
  def applyInnerProcedure(a: Expr, b: Expr): Expr = {
    val aInt = a.numValue
    val bInt = b.numValue
    val c = innerProcedure(aInt, bInt)
    Bool(c.asInstanceOf[Boolean])
  }

  def innerProcedure(a: Int, b: Int): Boolean
}

case class Sum(args: Expr*) extends NumericPrimitiveProcedure(args:_*) {
  def innerProcedure(a: Int, b: Int): Int = a + b
}

case class Prod(args: Expr*) extends NumericPrimitiveProcedure(args:_*) {
  def innerProcedure(a: Int, b: Int): Int = a * b
}

case class Less(args: Expr*) extends BooleanPrimitiveProcedure(args:_*) {
  def innerProcedure(a: Int, b: Int): Boolean = a < b
}

case class IfElse(args: Expr*) extends PrimitiveProcedure(args:_*) {
  override def apply(args: Expr*): Expr = {
    val cond = args(0)
    val thenExpr = args(1)
    val elseExpr = args(2)

    if (cond.boolValue) thenExpr
    else elseExpr
  }

  def applyInnerProcedure(a: Expr, b: Expr): Expr =
    throw new Error("IfElse should not implement applyInnerProcedure")
}