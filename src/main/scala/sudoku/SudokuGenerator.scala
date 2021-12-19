package sudoku

import scala.util.Random
import solver.DancingLinks

import scala.collection.immutable.Range.Inclusive

/**
 * Generates random sudokus with the help of sudoku solvers.
 */
object SudokuGenerator:

   private lazy val random: Random = Random()

   /**
    * Generates a randomly generated sudoku with all fields filled out, using something
    * very similar to the backtracking algorithm.
    * @return the sudoku.
    */
   def getValidSudoku: Sudoku =
      def innerBacktrack(field: Int, sudoku: Sudoku): Option[Sudoku] =
         if field == 81 then return Some(sudoku) // if at field 81 the sudoku must be valid.
         Random.shuffle(Seq.range(1, 10)).foreach { i => // shuffle the values to try.
            val newSudoku = sudoku.updated(field, i)
            if newSudoku.isValid then // if the new sudoku is valid access new layer.
               val ret = innerBacktrack(field + 1, newSudoku)
               if ret.isDefined then return ret
         }
         None
      innerBacktrack(0, Sudoku(Array.ofDim(81))).orNull

   /**
    * Generates a random sudoku with a certain amount of clues. Therefore it keeps track of all
    * fields where are still clues left and removes them one by one randomly.
    * The resulting sudoku is valid and has only one distinct solution.
    * @param clues indicates how many clues the resulting sudoku has, in other words it has
    *              81 - clues zeros.
    * @return The resulting sudoku.
    */
   def getSudoku(clues: Int): Sudoku =
      def backtrackSudoku(fields: Seq[(Int, Boolean)], depth: Int, sudoku: Sudoku): Option[Sudoku] =
         if 81 - depth == clues then return Some(sudoku) // if enough fields have been removed, return.
         else Random.shuffle(fields.filter(_._2)).foreach { (i, _) => // shuffle the remaining clues
            val newSudoku = sudoku.updated(i, 0)
            // if the reduced sudoku is still distinct call the next layer and return the result if its definied.
            if DancingLinks(newSudoku).isDistinct then
               val ret = backtrackSudoku(fields.updated(i, (i, false)), depth + 1, newSudoku)
               if ret.isDefined then return ret
         }
         None
      backtrackSudoku(Seq.iterate((0, true), 81)((i, _) => (i + 1, true)), 0, getValidSudoku).get

   /**
    * Iterator over all solutions with a certain amount of clues based on random starting sudoku,
    * therefore the sudokus are sometimes not distinct and do not differ very much.
    * @param clues indicates how many clues the resulting sudoku should have.
    * @return a Iterator over all possible sudokus.
    */
   def iterateSudokus(clues: Int): Iterator[Sudoku] =
      def iterateIntern(fields: Seq[(Int, Boolean)], depth: Int, sudoku: Sudoku): Iterator[Sudoku] =
         new Iterator[Sudoku] {

            private lazy val isSolution = 81 - depth == clues
            private var hasValue = true

            private lazy val childIterator: Iterator[(Int, Boolean)] = Random.shuffle(fields.filter(_._2)).to(Iterator)
            private var current: Option[Iterator[Sudoku]] = None

            private def proceed(): Boolean =
               while childIterator.hasNext do
                  val (i, _) = childIterator.next()
                  val newSudoku = sudoku.updated(i, 0)
                  if newSudoku.isValid && DancingLinks(newSudoku).isDistinct then
                     current = Some(iterateIntern(fields.updated(i, (i, false)), depth + 1, newSudoku))
                     if current.get.hasNext then return true
               false

            override def hasNext: Boolean =
               if isSolution then hasValue
               else if current.isEmpty || !current.get.hasNext then proceed()
                  else if !childIterator.hasNext then false
                  else true

            override def next(): Sudoku =
               if isSolution then
                  hasValue = false
                  sudoku
               else current.get.next()

         }
      iterateIntern(Seq.iterate((0, true), 81)((i, _) => (i + 1, true)), 1, getValidSudoku)