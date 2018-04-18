object Main extends App {
  val machine = new Machine
  val env = Map("x" -> 3, "y" -> 1, "b" -> true)

  val stat = Assign("z", Sum(Number(3), Number(2)))

  val expected = Map("x" -> 3, "y" -> 1, "b" -> true, "z" -> 5)
  val actual = machine.run(Assign("x", Sum(Var("x"), Number(1))), env)

  println(actual)
}