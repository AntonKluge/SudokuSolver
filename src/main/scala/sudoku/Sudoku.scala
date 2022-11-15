package sudoku

import solver.DancingLinks
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
    * @param string is a string in format 021012312\n782342378\n42 .... 234 for each row.
    * */
   def this(string: String) = this(string.linesIterator.flatMap(_.split("").map(_.toInt)).toArray)

   def getField(row: Int, column: Int): Int = sudoku(row * 9 + column)

   def getField(field: Int): Int = sudoku(field)

   def isRowValid(row: Int): Boolean = validCollection(getRow(row))

   def isColumnValid(column: Int): Boolean = validCollection(getColumn(column))

   def isBoxValid(box: Int): Boolean = validCollection(this.getBox(box))

   private def extractBox(box: Int, sudoku: Array[Int]): Array[Int] = (box match
      case 0 | 1 | 2 => sudoku.drop(box * 3)
      case 3 | 4 | 5 => sudoku.drop(27 + (box - 3) * 3)
      case 6 | 7 | 8 => sudoku.drop(54 + (box - 6) * 3)
      ).sliding(3, 9).take(3).flatten.toSeq.toArray

   private def extractBox2(box: Int, sudoku: Array[(Int, Int)]): Array[(Int, Int)] = (box match
      case 0 | 1 | 2 => sudoku.drop(box * 3)
      case 3 | 4 | 5 => sudoku.drop(27 + (box - 3) * 3)
      case 6 | 7 | 8 => sudoku.drop(54 + (box - 6) * 3)
      ).sliding(3, 9).take(3).flatten.toSeq.toArray

   def getBox(box: Int): Array[Int] =
      extractBox(box, sudoku)

   def getIndexedBox(box: Int): Array[(Int, Int)] =
      extractBox2(box, sudoku.zipWithIndex)

   private def extractColumn(column: Int, sudoku: Array[Int]): Array[Int] =
      sudoku.drop(column).sliding(1, 9).map(_ (0)).toArray

   private def extractColumn2(column: Int, sudoku: Array[(Int, Int)]): Array[(Int, Int)] =
      sudoku.drop(column).sliding(1, 9).map(_ (0)).toArray

   def getColumn(column: Int): Array[Int] =
      extractColumn(column, sudoku)

   def getIndexedColumn(column: Int): Array[(Int, Int)] =
      extractColumn2(column, sudoku.zipWithIndex)

   private def extractRow[A](row: Int, sudoku: Array[A]): Array[A] =
      sudoku.slice(row * 9, row * 9 + 9)

   def getRow(row: Int): Array[Int] =
      extractRow(row, sudoku)

   def getIndexedRow(row: Int): Array[(Int, Int)] =
      extractRow(row, sudoku.zipWithIndex)

   def isValid: Boolean =
      0 to 8 forall(n => isRowValid(n) && isColumnValid(n) && isBoxValid(n))

   def isSolved: Boolean =
      isValid && !sudoku.contains(0)

   /**
    * Checks of every none zero element in the given collection only occurs once.
    * @param partSudoku is a collection, probably a part of a sudoku to check.
    * @return if the collection is unique, in the context of sudokus if its valid.
    */
   private def validCollection(partSudoku: Array[Int]): Boolean =
      val mSet = scala.collection.mutable.Set[Int]()
      partSudoku.forall(x => if x == 0 then true else if mSet(x) then false else {mSet += x; true})

   def updated(field: Int, value: Int): Sudoku =
      Sudoku(sudoku.updated(field, value))

   def updated(row: Int, column: Int, value: Int): Sudoku =
      Sudoku(sudoku.updated(row * 9 + column, value))

   def getGrid: Array[Int] = sudoku

   /**
    * Numbers a this sudoku by mapping every field to its value and a tuple of coordinates.
    * @return a array of tuples with format (number in field, (fields row, fields column, fields box))
    */
   def getNumberedGrid: Array[(Int, (Int, Int, Int))] =
      sudoku.zip((0 to 8).flatMap(r => (0 to 8).map(c => (r, c, Sudoku.whichBox(r * 9 + c)))))

   def solve: Option[Sudoku] = DancingLinks(this).solve

   override def toString: String = sudoku.grouped(27).map(_.grouped(9).map(_.grouped(3).map(_.mkString(" "))
      .mkString(" | ")).map(_ + "\n").mkString).mkString("——————+———————+——————\n")
   
   override def equals(obj: Any): Boolean = obj match
      case Sudoku(arr) => arr.sameElements(sudoku)
      case _ => false
      
   
object Sudoku:

   /**
    * Calculates the coordinates of a given field, indexed 0 to 80.
    * @param field the field for witch the cords are returned.
    * @return A tuple of (fields row, fields column, field box)
    */
   def fieldToCords(field: Int): (Int, Int, Int) = (field / 9, field % 9,
      if field < 27 then (field % 9) / 3 else if field < 54 then (field % 9) / 3 + 3 else (field % 9) / 3 + 6)

   def whichBox(field: Int): Int =
      val colBox = field % 9 match
         case 0 | 1 | 2 => 0
         case 3 | 4 | 5 => 1
         case 6 | 7 | 8 => 2
      val rowBox = if field < 27 then 0 else if field < 54 then 1 else 2
      rowBox * 3 + colBox


