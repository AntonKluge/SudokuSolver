package sudoku

import scala.util.Random
import solver.DancingLinks

import scala.collection.immutable.Range.Inclusive

object SudokuGenerator:

   private lazy val random: Random = Random()

   def getValidSudoku: Sudoku =
      def innerBacktrack(field: Int, sudoku: Sudoku): Option[Sudoku] =
         if field == 81 then return Some(sudoku)
         Random.shuffle(Seq.range(1, 10)).foreach { i =>
            val newSudoku = sudoku.updated(field, i)
            if newSudoku.isValid then
               val ret = innerBacktrack(field + 1, newSudoku)
               if ret.isDefined then return ret
         }
         None
      innerBacktrack(0, Sudoku.sudokuZeros).orNull

   def getSudoku(clues: Int): Sudoku =
      def backtrackSudoku(fields: Seq[(Int, Boolean)], depth: Int, sudoku: Sudoku): Option[Sudoku] =
         if 81 - depth <= clues then return Some(sudoku)
         else Random.shuffle(fields.filter(_._2)).foreach { (i, _) =>
            val newSudoku = sudoku.updated(i, 0)
            if newSudoku.isValid && DancingLinks(newSudoku).isValid then
               val ret = backtrackSudoku(fields.updated(i, (i, false)), depth + 1, newSudoku)
               if ret.isDefined then return ret
         }
         None
      backtrackSudoku(Seq.iterate((0, true), 81)((i, _) => (i + 1, true)), 1, getValidSudoku).get
   
   def iterateSudokus(clues: Int): Iterator[Sudoku] =
      def iterateIntern(fields: Seq[(Int, Boolean)], depth: Int, sudoku: Sudoku): Iterator[Sudoku] =
         new Iterator[Sudoku] {

            private lazy val isSolution = 81 - depth <= clues
            private var hasValue = true

            private lazy val childIterator: Iterator[(Int, Boolean)] = Random.shuffle(fields.filter(_._2)).to(Iterator)
            private var current: Option[Iterator[Sudoku]] = None

            private def proceed(): Boolean =
               while childIterator.hasNext do
                  val (i, _) = childIterator.next()
                  val newSudoku = sudoku.updated(i, 0)
                  if newSudoku.isValid && DancingLinks(newSudoku).isValid then
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