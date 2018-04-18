# TinyLang

**Example:** Computing the factorial with TinyLang

```Scala
val machine = new Machine
val env = Map("x" -> 5)

val code = Sequence(
  Assign("factorial", Number(1)),
  While(
    Less(Number(1), Var("x")),
    Sequence(
      Assign("factorial", Prod(Var("factorial"), Var("x"))),
      Assign("x", Sum(Var("x"), Number(-1)))
    )
  )
)

machine.run(code, env)
//res0: Map[String,Any] = Map(x -> 1, factorial -> 120)
```
