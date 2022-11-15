package solver

import sudoku.Sudoku

/**
 * Use of backtracking to solve the sudoku in this class.
 * @param sudoku the sudoku to work on, its not being changed.
 */
class Backtracking(sudoku: Sudoku) extends Solver(sudoku) :

   /**
    * A simple backtracking algorithm without any heuristics to optimise it. It just goes
    * through field by field and tries out, every new layer of recursion the sudoku is
    * duplicated to keep a functional style, it doesn't really influences the performance.
    *
    *  @return a Option with the solved sudoku if there is a solution, otherwise None
    */
   override def solve: Option[Sudoku] =
      def nextField(field: Int, sudokuInner: Sudoku): Option[Sudoku] =
         if field == -1 then // if field 81 was called there must be 81 valid fields, so solved.
            Some(sudokuInner)
         else if !sudokuInner.isValid then 
            None
         else if sudokuInner.getField(field) != 0 then
            nextField(field - 1, sudokuInner) // skip clue fields.
         else
            (1 to 9).foldLeft (Option.empty) { (opt, n) => opt match
               case Some(s) => Some(s)
               case None => nextField(field - 1, sudokuInner.updated(field, n))
            }
      nextField(80, sudoku) // starting in the first field.