package solver

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.wordspec.AnyWordSpec

import sudoku.Sudoku

class DancingLinksTest extends AnyWordSpec:

   val sudokuEasy = new Sudoku("640298507\n052106984\n798045062\n903614870\n086530429\n574082603\n830769241\n419803756\n207451308")
   val sudokuMedium = new Sudoku("140760000\n000021600\n706000190\n053106800\n400000500\n690050020\n582697000\n000200058\n000500000")
   val sudokuHard = new Sudoku("000000070\n060300105\n304000200\n050096020\n008005000\n400800007\n531900000\n009000080\n020600450")
   val sudokuHell = new Sudoku("000040000\n100000060\n090807300\n006204030\n000090400\n020050000\n080302700\n000500000\n009000008")

   val sudokuEasySolution = new Sudoku("641298537\n352176984\n798345162\n923614875\n186537429\n574982613\n835769241\n419823756\n267451398")
   val sudokuMediumSolution = new Sudoku("145769283\n839421675\n726385194\n253176849\n478932516\n691854327\n582697431\n964213758\n317548962")
   val sudokuHardSolution = new Sudoku("185429673\n962378145\n374561298\n753196824\n218745936\n496832517\n531984762\n649257381\n827613459")
   val sudokuHellSolution = new Sudoku("238146579\n147935862\n695827341\n856214937\n713698425\n924753186\n581362794\n472589613\n369471258")

   "solution must be correct" must {
      "for easy sudokus" in {
         assert(DancingLinks(sudokuEasy).solve.get.equals(sudokuEasySolution))
      }
      "for medium sudokus" in {
         assert(DancingLinks(sudokuMedium).solve.get.equals(sudokuMediumSolution))
      }
      "for hard sudokus" in {
         assert(DancingLinks(sudokuHard).solve.get.equals(sudokuHardSolution))
      }
      "for hell sudokus" in {
         assert(DancingLinks(sudokuHell).solve.get.equals(sudokuHellSolution))
      }
   }

   "there must be one solution" must {
      "for the easy sudoku" in {
         assert(DancingLinks(sudokuEasy).isDistinct)
         assert(DancingLinks(sudokuEasy).solveIterator.size == 1)
      }
      "for the medium sudoku" in {
         assert(DancingLinks(sudokuMedium).isDistinct)
         assert(DancingLinks(sudokuMedium).solveIterator.size == 1)
      }
      "for the hard sudoku" in {
         assert(DancingLinks(sudokuHard).isDistinct)
         assert(DancingLinks(sudokuHard).solveIterator.size == 1)
      }
      "for the hell sudoku" in {
         assert(DancingLinks(sudokuHell).isDistinct)
         assert(DancingLinks(sudokuHell).solveIterator.size == 1)
      }
   }
