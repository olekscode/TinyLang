trait Stat
case class DoNothing() extends Stat
case class Assign(varName: String, value: Expr) extends Stat
case class If(cond: Expr, thenStat: Stat, elseStat: Stat) extends Stat
case class Sequence(stats: Stat*) extends Stat
case class While(cond: Expr, thenStat: Stat) extends Stat