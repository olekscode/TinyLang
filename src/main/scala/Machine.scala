final class Machine {
  def run(stat: Stat, env: Map[String, Any]): Map[String, Any] =
    stat match {
      case _: DoNothing => env
      case _: Assign => bindVariable(
        stat.asInstanceOf[Assign].varName,
        stat.asInstanceOf[Assign].value,
        env)
      case _: If => runIf(
        stat.asInstanceOf[If].cond,
        stat.asInstanceOf[If].thenStat,
        stat.asInstanceOf[If].elseStat,
        env)
      case _: Sequence => runSequence(
        stat.asInstanceOf[Sequence].stats,
        env)
      case _: While => runWhile(
        stat.asInstanceOf[While].cond,
        stat.asInstanceOf[While].thenStat,
        env)
      case _ => throw new Error("Unsupported type of statement")
    }

  def reduce(expr: Expr, env: Map[String, Any]): Expr = {
    //println(expr)

    if (expr.isReducible)
      reduce(reductionStep(expr, env), env)
    else
      expr
  }

  def reductionStep(expr: Expr, env: Map[String, Any]): Expr =
    expr match {
      case _: SelfEvaluatingExpr => expr
      case _: Var => lookupVariable(expr, env)
      case _: Procedure => procedureReductionStep(expr, expr.arguments, env)
      case _ => throw new Error("Unsupported type of expression")
    }

  def procedureReductionStep(proc: Expr, args: Seq[Expr], env: Map[String, Any]): Expr = {
    def findFirstReducible(head: Expr, tail: Seq[Expr], i: Int): Int =
      if (head.isReducible) i
      else if (tail.isEmpty) -1
      else findFirstReducible(tail.head, tail.tail, i + 1)

    val i = findFirstReducible(args.head, args.tail, 0)

    if (i == -1) apply(proc, args, env)
    else {
      val reduced = reductionStep(args(i), env)
      val reducedArgs = args updated (i, reduced)
    
      proc match {
        case _: Sum => Sum(reducedArgs:_*)
        case _: Prod => Prod(reducedArgs:_*)
        case _: Less => Less(reducedArgs:_*)
        case _: IfElse => IfElse(reducedArgs:_*)
        case _ => throw new Error("Unsupported type of procedure")
      }
    }
  }

  def apply(proc: Expr, args: Seq[Expr], env: Map[String, Any]): Expr =
    proc match {
      case _: PrimitiveProcedure => applyPrimitiveProcedure(proc, args, env)
      case _ => throw new Error("Compound procedures are not supported")
    }

  def applyPrimitiveProcedure(proc: Expr, args: Seq[Expr], env: Map[String, Any]): Expr =
    proc.apply(args:_*)

  def lookupVariable(expr: Expr, env: Map[String, Any]): Expr = {
    val name = expr.name
    val value =
      try {
        env(name)
      }
      catch {
        case _: java.util.NoSuchElementException =>
          throw new Error("Variable '$name' is unbound in the given environment")
      }

    value match {
      case _: Int => Number(value.asInstanceOf[Int])
      case _: Boolean => Bool(value.asInstanceOf[Boolean])
      case _ => throw new Error("Variable '$name' is bound to the value of unsupported type")
    }
  }

  def bindVariable(name: String, expr: Expr, env: Map[String, Any]): Map[String, Any] =
    try {
      val reducedExpr: Expr = reduce(expr, env)

      val value = reducedExpr match {
        case _: Number => reducedExpr.numValue
        case _: Bool => reducedExpr.boolValue
        case _ => throw new Error("Expression was evaluated to the value of unknown type")
      }
      env + (name -> value)
    }
    catch {
      case _: Error => env + ("___error" -> "Assignment failed.")
    }

  def runIf(cond: Expr, thenStat: Stat, elseStat: Stat,  env: Map[String, Any]): Map[String, Any] =
    try {
      val reducedCond: Expr = reduce(cond, env)

      val condValue = reducedCond match {
        case _: Bool => reducedCond.boolValue
        case _ => throw new Error("Expression was evaluated to a non-Boolean type")
      }
      
      if (condValue) run(thenStat, env)
      else run(elseStat, env)
    }
    catch {
      case _: Error => env + ("___error" -> "If statement failed.")
    }

  def runSequence(stats: Seq[Stat], env: Map[String, Any]): Map[String, Any] =
    if (stats.isEmpty) env
    else {
      val newEnv = run(stats.head, env)

      if (newEnv.contains("___error")) newEnv
      else runSequence(stats.tail, newEnv)
    }

  def runWhile(cond: Expr, thenStat: Stat, env: Map[String, Any]): Map[String, Any] =
    try {
      val reducedCond: Expr = reduce(cond, env)

      val condValue = reducedCond match {
        case _: Bool => reducedCond.boolValue
        case _ => throw new Error("Expression was evaluated to a non-Boolean type")
      }
      
      if (condValue) {
        val newEnv = run(thenStat, env)

        if (newEnv.contains("___error")) newEnv
        else runWhile(cond, thenStat, newEnv)
      }
      else env
    }
    catch {
      case _: Error => env + ("___error" -> "While statement failed.")
    }
  }