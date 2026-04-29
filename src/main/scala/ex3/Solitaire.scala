package ex3

object Solitaire extends App:
  private def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val rows =
      for y <- 1 to height
          row = for x <- 1 to width
                    number = solution.indexOf((x, y)) + 1
          yield if number > 0 then "%-3d".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  private val moves: Seq[(Int, Int)] = Seq(
    (-3, 0), (3, 0), (0, -3), (0, 3),
    (-2, -2), (-2, 2), (2, -2), (2, 2)
  )

  private def placeMarks(board: (Int, Int)): Iterable[Seq[(Int, Int)]] =
    val (w, h) = board
    val center = ((w + 1) / 2, (h + 1) / 2)

    def isValid(mark: (Int, Int), marks: Seq[(Int, Int)]): Boolean =
      val (x, y) = mark
      x >= 1 && x <= w &&
        y >= 1 && y <= h &&
        !marks.contains(mark)

    def place(n: Int): Iterable[Seq[(Int, Int)]] = n match
      case 1 => LazyList(Seq(center))
      case _ =>
        for
          marks <- place(n - 1)
          (x, y) = marks.last
          (dx, dy) <- moves
          mark = (x + dx, y + dy)
          if isValid(mark, marks)
        yield
          marks :+ mark

    place(w * h)

  for sol <- placeMarks((5, 5)) do
    println(render(solution = sol, width = 5, height = 5))
    println("---")