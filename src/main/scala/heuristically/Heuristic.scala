package heuristically

import sudoku.Sudoku

trait Heuristic:

   private lazy val heuristics = Seq(
      OneFreeField
   ).map(_.check)

   /** 
    * Checks whether the implementing heuristic can add more entries, and does so if possible.
    *
    * @param sudoku the sudoku to update.
    * @return if some entries could be updated it returns the updated sudoku as Some, else None.
    */
   def check(sudoku: Sudoku): Option[Sudoku]

   def checkAll(sudoku: Sudoku): Option[Sudoku] =
      heuristics.foldLeft (Option.empty) { (opt, heuristic) => opt match
         case None => heuristic(sudoku)
         case Some(updatedSudoku) => heuristic(updatedSudoku)
      }
