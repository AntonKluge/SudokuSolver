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

   def update(field: Int, value: Int): Sudoku =
      sudoku.update(field, value)
      this

   def getGrid: Array[Int] = sudoku

   def getNumberedGrid: Array[(Int, (Int, Int, Int))] =
      sudoku.zip((0 to 8).flatMap(r => (0 to 8).map(c => (r, c, Sudoku.whichBox(r * 9 + c)))))

   def randomNextField: Int =
      val free = sudoku.zip(0 to 80).filter(_._1 == 0)
      if free.length == 0 then -1
      else free(Random().nextInt(free.length))._2

   override def toString: String = sudoku.sliding(9, 9).map(_.mkString(" | ")).mkString("\n")


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