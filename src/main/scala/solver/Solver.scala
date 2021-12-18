package solver

import sudoku.Sudoku

/**
 *
 */
trait Solver(sudoku: Sudoku):

   /**
    *
    * @return
    */
   def solve: Option[Sudoku]