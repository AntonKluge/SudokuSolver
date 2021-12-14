package solver

import solver.Solver
import sudoku.Sudoku

class Backtracking(sudoku: Sudoku) extends Solver(sudoku):

  override def solve: Sudoku =
    def nextField(field: Int): Boolean = if field == 81 then true
      else if sudoku.getField(field) != 0 then nextField(field + 1)
      else if (1 to 9).exists(n => sudoku.update(field, n).isValid && nextField(field + 1)) then true
      else { sudoku.update(field, 0); false }
    if nextField(0) then sudoku else null
