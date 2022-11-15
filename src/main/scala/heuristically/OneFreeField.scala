package heuristically

import sudoku.Sudoku

object OneFreeField extends Heuristic() :

   /** One free tile
    *
    * If only one field in the row, column or block is free, it fills it
    * with the missing number.
    *
    * @param sudoku the sudoku to update.
    * @return if some fields could be updated it returns the updated sudoku as Some, else None.
    */
   override def check(sudoku: Sudoku): Option[Sudoku] =
      1 to 9 foreach { i =>
        i

      }

      None

