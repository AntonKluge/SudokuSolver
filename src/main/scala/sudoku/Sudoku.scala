package sudoku

import scala.util.Random

/**
 * A utility class for sudokus which saves and manipulates a given sudoku in array form
 * and provides multiple helpful utility functions to work on it.
 *
 * @param sudoku an array of length 81 which saves the numbers 1-9 and 0 if no clue yet row by row.
 */
case class Sudoku(sudoku: Array[Int]):

   /**
    * Parses a sudoku from a specified string format.
    *
    * @param string is a string in format 021012312\n782342378\n42 .... 234 for each row.
    */
   def this(string: String) = this(string.linesIterator.flatMap(_.split("").map(_.toInt)).toArray)

   /**
    * Gets a field from this sudoku, indexed by row and column
    *
    * @param row    the row you want from the number from, 0 to 8 indexed.
    * @param column the column you want from the number from, 0 to 8 indexed.
    * @return the fields number 1 to 9 if there is a number and 0 if theres no number.
    */
   def getField(row: Int, column: Int): Int = sudoku(row * 9 + column)

   /**
    * Gets a field, indexed by 9 * row + column.
    *
    * @param field the field in the array.
    * @return the fields number 1 to 9 if there is a number and 0 if theres no number.
    */
   def getField(field: Int): Int = sudoku(field)

   def isRowValid(row: Int): Boolean =
      validCollection(Iterator.from(sudoku.slice(row * 9, row * 9 + 9)))

   def isColumnValid(column: Int): Boolean =
      validCollection(sudoku.drop(column).sliding(1, 9).map(_ (0)))

   def isBoxValid(box: Int): Boolean = validCollection(this.getBox(box))

   def getBox(box: Int): Iterator[Int] = (box match
      case 0 | 1 | 2 => sudoku.drop(box * 3)
      case 3 | 4 | 5 => sudoku.drop(27 + (box - 3) * 3)
      case 6 | 7 | 8 => sudoku.drop(54 + (box - 6) * 3)
      ).sliding(3, 9).take(3).flatten

   def isValid: Boolean = (0 to 8).forall(n => isRowValid(n) && isColumnValid(n) && isBoxValid(n))

   private def validCollection(partSudoku: Iterator[Int]): Boolean =
      val mSet = scala.collection.mutable.Set[Int]()
      partSudoku.forall(x => if x == 0 then true else if mSet(x) then false else {mSet += x; true})

   def updated(field: Int, value: Int): Sudoku = Sudoku(sudoku.updated(field, value))

   def getGrid: Array[Int] = sudoku

   def getNumberedGrid: Array[(Int, (Int, Int, Int))] =
      sudoku.zip((0 to 8).flatMap(r => (0 to 8).map(c => (r, c, Sudoku.whichBox(r * 9 + c)))))

   def randomNextField: Int =
      val free = sudoku.zip(0 to 80).filter(_._1 == 0)
      if free.length == 0 then -1
      else free(Random().nextInt(free.length))._2

   override def toString: String = ("%d %d %d | %d %d %d | %d %d %d\n" + "%d %d %d | %d %d %d | %d %d %d\n" +
         "%d %d %d | %d %d %d | %d %d %d\n" + "—————————————————————\n" + "%d %d %d | %d %d %d | %d %d %d\n" +
         "%d %d %d | %d %d %d | %d %d %d\n" + "%d %d %d | %d %d %d | %d %d %d\n" + "—————————————————————\n" +
         "%d %d %d | %d %d %d | %d %d %d\n" + "%d %d %d | %d %d %d | %d %d %d\n" + "%d %d %d | %d %d %d | %d %d %d")
      .format(sudoku(0), sudoku(1), sudoku(2), sudoku(3), sudoku(4), sudoku(5), sudoku(6), sudoku(7), sudoku(8),
         sudoku(9), sudoku(10), sudoku(11), sudoku(12), sudoku(13), sudoku(14), sudoku(15), sudoku(16), sudoku(17),
         sudoku(18), sudoku(19), sudoku(20), sudoku(21), sudoku(22), sudoku(23), sudoku(24), sudoku(25), sudoku(26),
         sudoku(27), sudoku(28), sudoku(29), sudoku(30), sudoku(31), sudoku(32), sudoku(33), sudoku(34), sudoku(35),
         sudoku(36), sudoku(37), sudoku(38), sudoku(39), sudoku(40), sudoku(41), sudoku(42), sudoku(43), sudoku(44),
         sudoku(45), sudoku(46), sudoku(47), sudoku(48), sudoku(49), sudoku(50), sudoku(51), sudoku(52), sudoku(53),
         sudoku(54), sudoku(55), sudoku(56), sudoku(57), sudoku(58), sudoku(59), sudoku(60), sudoku(61), sudoku(62),
         sudoku(63), sudoku(64), sudoku(65), sudoku(66), sudoku(67), sudoku(68), sudoku(69), sudoku(70), sudoku(71),
         sudoku(72), sudoku(73), sudoku(74), sudoku(75), sudoku(76), sudoku(77), sudoku(78), sudoku(79), sudoku(80))

   override def clone(): Sudoku = Sudoku(sudoku.clone())

object Sudoku:

   def fieldToCords(field: Int): (Int, Int, Int) = (field / 9, field % 9,
      if field < 27 then (field % 9) / 3 else if field < 54 then (field % 9) / 3 + 3 else (field % 9) / 3 + 6)

   def whichBox(field: Int): Int =
      val colBox = field % 9 match
         case 0 | 1 | 2 => 0
         case 3 | 4 | 5 => 1
         case 6 | 7 | 8 => 2
      val rowBox = if field < 27 then 0 else if field < 54 then 1 else 2
      rowBox * 3 + colBox

   def sudokuZeros: Sudoku = new Sudoku("000000000\n000000000\n000000000\n" +
      "000000000\n000000000\n000000000\n000000000\n000000000\n000000000")

   def sudokuEasy: Sudoku = new Sudoku("640298507\n052106984\n798045062" +
      "\n903614870\n086530429\n574082603\n830769241\n419803756\n207451308")

   def sudokuMedium: Sudoku = new Sudoku("140760000\n000021600\n706000190\n" +
      "053106800\n400000500\n690050020\n582697000\n000200058\n000500000")

   def sudokuHard: Sudoku = new Sudoku("000000070\n060300105\n304000200\n" +
      "050096020\n008005000\n400800007\n531900000\n009000080\n020600450")

   def sudokuHell: Sudoku = new Sudoku("000040000\n100000060\n090807300\n" +
      "006204030\n000090400\n020050000\n080302700\n000500000\n009000008")

   def sudokuAntiBruteForce: Sudoku = new Sudoku("000000000\n000003085\n" +
      "001020000\n000507000\n004000100\n090000000\n500000073\n002010000\n000040009")

