package solver

import sudoku.Sudoku

/**
 *
 * @param sudoku
 */
class Backtracking(sudoku: Sudoku) extends Solver(sudoku) :

   override def solve: Option[Sudoku] =
      def nextField(field: Int, sudokuInner: Sudoku): Option[Sudoku] =
         if field == 81 then
            Some(sudokuInner)
         else if sudokuInner.getField(field) != 0 then
            nextField(field + 1, sudokuInner)
         else
            (1 to 9).foreach { n =>
               val newSudoku = sudokuInner.updated(field, n)
               if newSudoku.isValid then
                  val ret = nextField(field + 1, newSudoku)
                  if ret.isDefined then return ret
            }
            None
      nextField(0, sudoku)