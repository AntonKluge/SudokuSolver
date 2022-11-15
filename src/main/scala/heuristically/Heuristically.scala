package heuristically

import heuristically.OneFreeField.checkAll
import solver.Solver
import sudoku.Sudoku

import scala.annotation.tailrec

case class Heuristically(sudoku: Sudoku) extends Solver(sudoku) :

   override def solve: Option[Sudoku] =
      @tailrec def solveRec(sudokuRec: Sudoku): Option[Sudoku] =
         if sudokuRec.isSolved then Some(sudokuRec)
         else checkAll(sudoku) match
            case Some(value) => solveRec(value)
            case None => None
      solveRec(sudoku)


