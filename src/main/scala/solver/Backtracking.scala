package solver

import sudoku.Sudoku

/**
 *
 * @param sudoku
 */
class Backtracking(sudoku: Sudoku) extends Solver(sudoku) :

   override def solve: Sudoku =
      val sudokuWorkOn = sudoku.clone() // So we dont change the original sudoku
      def nextField(field: Int): Boolean =
         if field == 81 then
            true // if the field was called with 81 then we successfully finished
         else if sudokuWorkOn.getField(field) != 0 then
            nextField(field + 1) // if on the field is already a number: skip
         else if (1 to 9).exists(n => sudokuWorkOn.update(field, n).isValid && nextField(field + 1)) then 
            true // if one possibility is valid, we call the next field with it, if it returns true, so do we
         else
            sudokuWorkOn.update(field, 0) // if it was not successful, reset the field
            false // and return false
      if nextField(0) then sudokuWorkOn else null
