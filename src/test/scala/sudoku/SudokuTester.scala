package sudoku

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.wordspec.AnyWordSpec

class SudokuTester extends AnyWordSpec :

  "Sudoku field finder must" must {
    "find right field" in {
      assert(Sudoku.fieldToCords(0) == (0, 0, 0))
      assert(Sudoku.fieldToCords(1) == (0, 1, 0))
      assert(Sudoku.fieldToCords(2) == (0, 2, 0))
      assert(Sudoku.fieldToCords(3) == (0, 3, 1))
      assert(Sudoku.fieldToCords(4) == (0, 4, 1))
      assert(Sudoku.fieldToCords(5) == (0, 5, 1))
      assert(Sudoku.fieldToCords(6) == (0, 6, 2))
      assert(Sudoku.fieldToCords(7) == (0, 7, 2))
      assert(Sudoku.fieldToCords(8) == (0, 8, 2))

      assert(Sudoku.fieldToCords(9) == (1, 0, 0))
    }
  }

  "Sudoku box checker must" must {
    "return right box" in {
      val boxes = Seq(0, 0, 0, 1, 1, 1, 2, 2, 2, 0, 0, 0, 1, 1, 1, 2, 2, 2, 0, 0, 0, 1, 1, 1, 2, 2, 2,
        3, 3, 3, 4, 4, 4, 5, 5, 5, 3, 3, 3, 4, 4, 4, 5, 5, 5, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6,
        7, 7, 7, 8, 8, 8, 6, 6, 6, 7, 7, 7, 8, 8, 8, 6, 6, 6, 7, 7, 7, 8, 8, 8)
      (0 to 80).zip(boxes).foreach(t => assert(Sudoku.whichBox(t._1) == t._2))
    }
  }