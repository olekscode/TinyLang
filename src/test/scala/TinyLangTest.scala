import org.scalatest.FunSuite

class TinyLangTest extends FunSuite {
  val machine = new Machine
  val env = Map("x" -> 3, "y" -> 1, "b" -> true) // <= initial environment for the tests
  
  def reductionStep(expr: Expr): Expr =
    machine.reductionStep(expr, env)
  def reduce(expr: Expr): Expr =
    machine.reduce(expr, env)
  def run(stat: Stat): Map[String, Any] =
    machine.run(stat, env)

  /////////////////////////// EXPRESSIONS \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  // Number
  test("Number does not reduce") {
    assert(reduce(Number(3)) == Number(3))
  }
  // Bool
  test("Bool does not reduce") {
    assert(reduce(Bool(true)) == Bool(true))
  }
  // Var
  test("Number Var reduces to its value") {
    assert(reduce(Var("x")) == Number(3))
  }
  test("Bool Var reduces to its value") {
    assert(reduce(Var("b")) === Bool(true))
  }
  test("Unknown Var does not reduce") {
    intercept[Error] { reduce(Var("a")) }
  }
  
  // Sum
  test("Sum of two Numbers reduces to Number with their sum") {
    assert(reduce(Sum(Number(3), Number(4))) == Number(7))
  }
  test("Sum of Number and Bool does not reduce") {
    intercept[Error] { reduce(Sum(Number(3), Bool(true))) }
  }
  test("Sum of Bool and Number does not reduce") {
    intercept[Error] { reduce(Sum(Bool(true), Number(3))) }
  }
  test("left Sum operand reduces if it is reducible and right is left unchanged") {
    assert(reductionStep(
      Sum(
        Sum(Number(1), Number(2)),
        Sum(Number(3), Number(4)))) == 
      Sum(
        Number(3),
        Sum(Number(3), Number(4))))
  }
  test("otherwise right Sum operand reduces") {
    assert(reductionStep(
      Sum(
        Number(1),
        Sum(Number(3), Number(4)))) == 
      Sum(
        Number(1),
        Number(7)))
  }
  
  // Prod
  test("Prod of two Numbers reduces to Number with their product") {
    assert(reduce(Prod(Number(3), Number(4))) == Number(12))
  }
  test("Prod of Number and Bool does not reduce") {
    intercept[Error] { reduce(Prod(Number(3), Bool(true))) }
  }
  test("Prod of Bool and Number does not reduce") {
    intercept[Error] { reduce(Prod(Bool(true), Number(3))) }
  }
  test("left Prod operand reduces if it is reducible and right is left unchanged") {
    assert(reductionStep(
      Prod(
        Prod(Number(1), Number(2)),
        Prod(Number(3), Number(4)))) == 
      Prod(
        Number(2),
        Prod(Number(3), Number(4))))
  }
  test("otherwise right Prod operand reduces") {
    assert(reductionStep(
      Prod(
        Number(1),
        Prod(Number(3), Number(4)))) == 
      Prod(
        Number(1),
        Number(12)))
  }
  
  // Less
  test("Less of two Numbers reduces to Bool indicating whether first number is less than the second") {
    assert(reduce(Less(Number(3), Number(4))) == Bool(true))
  }
  test("Less of Number and Bool does not reduce") {
    intercept[Error] { reduce(Less(Number(3), Bool(true))) }
  }
  test("Less of Bool and Number does not reduce") {
    intercept[Error] { reduce(Less(Bool(true), Number(3))) }
  }
  test("left Less operand reduces if it is reducible and right is left unchanged") {
    assert(reductionStep(
      Less(
        Sum(Number(1), Number(2)),
        Sum(Number(3), Number(4)))) == 
      Less(
        Number(3),
        Sum(Number(3), Number(4))))
  }
  test("otherwise right Less operand reduces") {
    assert(reductionStep(
      Less(
        Number(1),
        Sum(Number(3), Number(4)))) == 
      Less(
        Number(1),
        Number(7)))
  }
  
  // IfElse
  test("IfElse reduces to thenExpr for Bool(true) condition") {
    assert(reduce(IfElse(Bool(true), Number(3), Number(4))) == Number(3))
  }
  test("IfElse reduces to elseExpr for Bool(false) condition") {
    assert(reduce(IfElse(Bool(false), Number(3), Number(4))) == Number(4))
  }
  test("IfElse for Number condition does not reduce") {
    intercept[Error] { reduce(IfElse(Number(1), Number(3), Number(4))) }
  }
  test("IfElse for reducible condition reduces its condition") {
    assert(reductionStep(IfElse(Less(Number(4), Number(3)), Number(3), Number(4))) ==
      IfElse(Bool(false), Number(3), Number(4)))
  }

  /////////////////////////// STATEMENTS (trait Stat) \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
  // DoNothing
  test("DoNothing does not alter environment") {
    assert(run(DoNothing()) == env)
  }
  
  // Assign
  test("Assign adds variable for number expression") {
    assert(run(Assign("z", Sum(Number(3), Number(2)))) ==
      Map("x" -> 3, "y" -> 1, "b" -> true, "z" -> 5))
  }
  test("Assign adds variable for boolean expression") {
    assert(run(Assign("b2", Less(Number(3), Number(2)))) ==
      Map("x" -> 3, "y" -> 1, "b" -> true, "b2" -> false))
  }
  test("Assign updates existing variable for number expression") {
    assert(run(Assign("b", Less(Number(3), Number(2)))) ==
      Map("x" -> 3, "y" -> 1, "b" -> false))
  }
  test("Assign updates existing variable for boolean expression") {
    assert(run(Assign("x", Sum(Number(3), Number(2)))) ==
      Map("x" -> 5, "y" -> 1, "b" -> true))
  }
  test("Assign updates existing variable for expression with the same variable") {
    assert(run(Assign("x", Sum(Var("x"), Number(1)))) ==
      Map("x" -> 4, "y" -> 1, "b" -> true))
  }
  test("Assign does not occur for erroneous expression") {
    assert(run(Assign("x", Sum(Bool(true), Number(1)))) ==
      Map("x" -> 3, "y" -> 1, "b" -> true, "___error" -> "Assignment failed."))
  }
  
  // If
  test("'If' runs thenStat if condition is Bool(true)") {
    assert(run(If(Less(Number(3), Number(4)), Assign("z", Number(1)), Assign("z", Number(2)))) ==
      Map("x" -> 3, "y" -> 1, "b" -> true, "z" -> 1))
  }
  test("'If' runs elseStat if condition is Bool(false)") {
    assert(run(If(Less(Number(4), Number(3)), Assign("z", Number(1)), Assign("z", Number(2)))) ==
      Map("x" -> 3, "y" -> 1, "b" -> true, "z" -> 2))
  }
  test("'If' statement fails for erroneous condition") {
    assert(run(If(Sum(Bool(true), Number(3)), Assign("z", Number(1)), Assign("z", Number(2)))) ==
      Map("x" -> 3, "y" -> 1, "b" -> true, "___error" -> "If statement failed."))
  }
  test("'If' statement fails for condition expression that reduces to Number") {
    assert(run(If(Sum(Number(4), Number(3)), Assign("z", Number(1)), Assign("z", Number(2)))) ==
      Map("x" -> 3, "y" -> 1, "b" -> true, "___error" -> "If statement failed."))
  }
  
  // Seq
  test("'Seq' does nothing if empty") {
    assert(run(Sequence()) == env)
  }
  test("'Seq' executes one its statement if contains only one") {
    assert(run(Sequence(Assign("z", Number(5)))) ==
      Map("x" -> 3, "y" -> 1, "b" -> true, "z" -> 5))
  }
  test("'Seq' executes its statements one by one") {
    assert(run(Sequence(
        Assign("x1", Sum(Var("x"), Number(1))),
        Assign("x2", Sum(Var("x1"), Number(1))))) ==
      Map("x" -> 3, "y" -> 1, "b" -> true, "x1" -> 4, "x2" -> 5))
  }
  test("'Seq' does not execute remained statements after first failure") {
    assert(run(Sequence(
        Assign("x1", Sum(Var("x"), Number(1))),
        Assign("x2", Var("z")),
        Assign("x3", Sum(Var("x1"), Number(1))))) ==
      Map("x" -> 3, "y" -> 1, "b" -> true, "x1" -> 4, "___error" -> "Assignment failed."))
  }

  // While
  test("'While' executes thenStat multiple times while condition reduces to Bool(true)") {
    assert(run(
      While(
        Less(Var("x"), Number(10)),
        Assign("x", Sum(Var("x"), Number(1)))
      )
    ) == Map("x" -> 10, "y" -> 1, "b" -> true))
  }
  test("'While' does not execute thenStat if condition reduces to Bool(false) from the start") {
    assert(run(
      While(
        Less(Var("x"), Number(0)),
        Assign("x", Var("z"))
      )
    ) == Map("x" -> 3, "y" -> 1, "b" -> true))
  }
  test("'While' statement fails for erroneous condition") {
    assert(run(
      While(
        Less(Var("x"), Bool(true)),
        Assign("x",  Sum(Var("x"), Number(1)))
      )
    ) == Map("x" -> 3, "y" -> 1, "b" -> true, "___error" -> "While statement failed."))
  }
  test("'While' statement fails for condition expression that reduces to Number") {
    assert(run(
      While(
        Sum(Var("x"), Number(10)),
        Assign("x",  Sum(Var("x"), Number(1)))
      )
    ) == Map("x" -> 3, "y" -> 1, "b" -> true, "___error" -> "While statement failed."))
  }
  test("'While' statement fails if thenStat statement fails") {
    assert(run(
      While(
        Less(Var("x"), Number(10)),
        Sequence(
          Assign("x", Sum(Var("x"), Number(1))),
          Assign("y", Var("z"))
        )
      )
    ) == Map("x" -> 4, "y" -> 1, "b" -> true, "___error" -> "Assignment failed."))
  }
}
