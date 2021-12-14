package solver

import sudoku.Sudoku

class ExactCover(sudoku: Sudoku) extends Solver(sudoku):

  private val exactCover = exactCoverMatrix()
  
  override def solve: Sudoku =
    val ecm = exactCoverMatrix()
    println(ecm.map(_.map(i => if i == 0 then " " else "X").mkString).mkString("\n"))
    null

  private def exactCoverMatrix(): Array[Array[Int]] =
    // creates a sudoku with (digit, (row, column)) to work on
    sudoku.getGrid.zip((0 to 8).flatMap(r => (0 to 8).map(c => (r, c)))).flatMap { field =>
      val (d, (r, c)) = field
      val b = Sudoku.whichBox(r * 9 + c)
      val arr = Array.ofDim[Int](9, 324)
      // colum row existing constraint
      if d != 0 then arr(d - 1)(r * 9 + c) = 1 else (0 to 8).foreach(nd => arr(nd)(r * 9 + c) = 1)
      Seq((81, r), (162, c), (243, b)).foreach { t => if d != 0 then arr(d - 1)(t._1 + t._2 * 9 + (d - 1))
        else (0 to 8).foreach(nd => arr(nd)(t._1 + t._2 * 9 + nd) = 1) }
      arr
    }
  
  private def algorithmX(): Unit = println("")


  private def coverRow(row: Int): Unit = println("")