
import solver.{Backtracking, DancingLinks}
import sudoku.Sudoku

object Main extends App :

   val sudokuZeros = new Sudoku("000000000\n000000000\n000000000\n000000000\n000000000\n000000000\n000000000\n000000000\n000000000")
   val sudokuEasy = new Sudoku("640298507\n052106984\n798045062\n903614870\n086530429\n574082603\n830769241\n419803756\n207451308")
   val sudokuMedium = new Sudoku("140760000\n000021600\n706000190\n053106800\n400000500\n690050020\n582697000\n000200058\n000500000")
   val sudokuHard = new Sudoku("000000070\n060300105\n304000200\n050096020\n008005000\n400800007\n531900000\n009000080\n020600450")
   val sudokuHell = new Sudoku("000040000\n100000060\n090807300\n006204030\n000090400\n020050000\n080302700\n000500000\n009000008")
   val sudokuAntiBruteForce = new Sudoku("000000000\n000003085\n001020000\n000507000\n004000100\n090000000\n500000073\n002010000\n000040009")

   Seq(sudokuEasy, sudokuMedium, sudokuHard, sudokuHell).foreach { sudoku =>

      var t1 = System.nanoTime
      val solved = Backtracking(sudoku).solve
      var duration = (System.nanoTime - t1) / 1e9d

      println("Took Backtracking: " + duration + "s")
      println(solved)

      t1 = System.nanoTime
      val solved2 = DancingLinks(sudoku).solve
      duration = (System.nanoTime - t1) / 1e9d

      println("Took Dancing Links: " + duration + "s")
      println(solved)

      if solved.toString != solved2.toString then throw IllegalArgumentException()
   }
   
   var t1 = System.nanoTime
   val solved = DancingLinks(sudokuAntiBruteForce).solve
   var duration = (System.nanoTime - t1) / 1e9d

   println("Took Dancing Links: " + duration + "s")
   println(solved)