package solver

import sudoku.Sudoku

/**
 * A trait which all heuristics to solve a sudoku should implement.
 */
trait Solver(sudoku: Sudoku):

   /**
    * Simply solve this solvers sudoku.
    * @return a solved sudoku as Some, if there is any, otherwise None.
    */
   def solve: Option[Sudoku]