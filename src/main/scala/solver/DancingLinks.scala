package solver

import sudoku.Sudoku

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * A algorithm to solve sudokus by converting them into exact cover problems and then
 * using algorithm X to solve them. The basics are a 729 x 324 grid of ones and zeros
 * containing all the constraints and possibilities.
 * The rows represent thereby the existence of a number N in row
 * 81 * rowSudoku + 9 * columnSudoku + (N - 1).
 * The columns represent constraints, with the first 81 being the constraint that
 * in each field must be at least one number, the second that in each row must be
 * one of each digit, the third that in each column must be one and the last that
 * in each box must be one number, all numbers from 0 o 9.
 *
 * Having this so called exact cover matrix, its transferred to a double linked grid
 * where only ones are stored.
 *
 * On this grid the algorithm x is used.
 *
 * @param sudoku The sudoku to solve.
 */
class DancingLinks(sudoku: Sudoku) extends Solver(sudoku) :

   private lazy val gridPub: Seq[Constraint] = linkedListGrid()

   override def solve: Option[Sudoku] = algorithmX()

   def solveN(n: Int): Seq[Sudoku] = iterSolutions.take(n).toSeq

   /** Implements a iterator over every solution @sudoku has. Here are multiple inner
    * iterators at play.
    * @return said iterator.
    */
   private def iterSolutions: Iterator[Sudoku] =
      def iterIntern(acc: Seq[LinkedNode]): Iterator[Sudoku] =
         new Iterator[Sudoku] {
            // if this is a solution we see it as a leaf.
            private val isSolution: Boolean = gridPub.forall(_.covered)
            private var valLeft: Boolean = true
            // the best column to cover, is only saved to uncover it in the end.
            private lazy val chosenColumn: Constraint = {
               val chosen = chooseColumn()
               chosen.cover()
               chosen
            }
            // all intersections of the chosenColumn with other rows.
            private lazy val children: Iterator[LinkedNode] = Iterator.from(chosenColumn.getColumn)
            // the current row which intersects with the chosenColumn.
            private var currentChild: Option[LinkedNode] = None
            // the iterator we get if we select a row from children, cover it and start a new recursive layer.
            private var currentChildIter: Option[Iterator[Sudoku]] = None
            // implements the selection of the new currentChild and the creation of the new currentChildIter
            private def proceed(): Boolean = {
               while children.hasNext do
                  currentChild = Some(children.next())
                  currentChild.get.cover()
                  currentChildIter = Some(iterIntern(acc :+ currentChild.get))
                  if currentChildIter.get.hasNext then return true
                  else currentChild.get.uncover()
               chosenColumn.uncover()
               false
            }

            override def hasNext: Boolean =
               if isSolution then
                  valLeft // if this is a leaf, just return if it has been iterated.
               else if currentChildIter.isEmpty || currentChild.isEmpty then
                  proceed() // if this node has never been called, proceed to the first element of its children.
               else if !currentChildIter.get.hasNext then
                  currentChild.get.uncover() // if currentChild is exhausted ..
                  if !children.hasNext then // .. and children is too then there is no more.
                     chosenColumn.uncover()
                     false
                  else
                     proceed() // .. and children is not just take the next child
               else
                  true

            override def next(): Sudoku =
               if isSolution then
                  valLeft = false
                  updatedSudoku(acc)
               else
                  currentChildIter.get.next()
         }

      iterIntern(Seq.empty)

   /**
    * @return Returns the most suitable columns for the next iteration in algorithm X,
    *         most suitable means the least ones in its column.
    */
   private def chooseColumn(): Constraint = gridPub.filter(!_.covered).minBy(_.seqVertical().size)

   /**
    * Writes the resulting digits into a new sudoku and returns it.
    * @param res A Seq of Nodes, representing a row in the solution.
    * @return The new sudoku, which is not linked to the classes sudoku.
    */
   private def updatedSudoku(res: Seq[LinkedNode]): Sudoku =
      res.foldLeft(sudoku)((fSudoku, node) => fSudoku.updated(node.field._1 * 9 + node.field._2, node.field._3))

   def solveIterator: Iterator[Sudoku] = iterSolutions

   def isDistinct: Boolean = iterSolutions.size == 1

   /**
    * Performs the algorithm X on a exact cover matrix.
    * @param accumulating the accumulated results, in the beginning start with a empty Seq.
    * @return if found a Seq of Linked nodes for every row in the result.
    */
   private def algorithmX(accumulating: Seq[LinkedNode] = Seq.empty[LinkedNode]): Option[Sudoku] =
      if gridPub.forall(_.covered) then
         Some(updatedSudoku(accumulating)) // if the grid is empty, we found a solution
      else
         val chosenCol = chooseColumn()
         chosenCol.cover() // chose the most suitable column
         // now go though all the rows which have a one it the column
         chosenCol.getColumn.foreach { node =>
            // and cover all columns which intersect with them.
            node.cover()
            val res = algorithmX(accumulating :+ node) // and start a new iteration
            if res.isDefined then return res // found result? return it!
            // otherwise uncover the covered lines and columns
            node.uncover()
         }
         chosenCol.uncover()
         None // and continue searching

   /**
    * Creates the cover matrix as double linked list grid, here we use the previously defined
    * Node classes.
    * @return A Seq of the constraint columns.
    */
   private def linkedListGrid(): Seq[Constraint] =
      val grid = Seq.iterate((Constraint(0), 1), 324)(t => // constraints for 0 to 324
         (t._1.linkRight(Constraint(t._2)), t._2 + 1)).map(_._1) // linked to each other
      sudoku.getNumberedGrid.foreach { field =>
         val (d, (r, c, b)) = field // match the digit, row, column and box out of field
         val calcPos = Seq((i: Int) => r * 9 + c, // calculate offset for at least one in every box
            i => 81 + r * 9 + i, // calculate offset for exactly one of each in every row
            i => 162 + c * 9 + i, // calculate offset for exactly one of each in every column
            i => 243 + b * 9 + i) // calculate offset for exactly one of each in every box
         (if d == 0 then 0 to 8 else Seq(d - 1)) // if theres a clue, we only create it, otherwise 0 to 8 possibilities
            .foreach(nd => calcPos.map(f => LinkedNode(grid(f(nd)), (r, c, nd + 1)))
               .reduceLeft((insTo, newNode) => insTo.linkRight(newNode))) // create them and link to each other (and up)
      }
      grid

   trait Node():

      var left, right, upper, lower = this

      /**
       * @param insertNode links the insertNode to the right of this (and adjusts the connected).
       * @tparam N a node, either Constraint or LinkedNode.
       * @return
       */
      def linkRight[N <: Node](insertNode: N): N =
         insertNode.right = right
         insertNode.right.left = insertNode
         insertNode.left = this
         right = insertNode
         insertNode

      /**
       * @param insertNode links the insertNode to the lower of this (and adjusts the connected).
       * @tparam N a node, either Constraint or LinkedNode.
       * @return
       */
      def linkLower[N <: Node](insertNode: N): N =
         insertNode.lower = lower
         insertNode.lower.upper = insertNode
         insertNode.upper = this
         lower = insertNode
         insertNode

      def removeHorizontal(): Unit =
         left.right = right
         right.left = left

      def removeVertical(): Unit =
         upper.lower = lower
         lower.upper = upper

      def reinsertHorizontal(): Unit =
         left.right = this
         right.left = this

      def reinsertVertical(): Unit =
         upper.lower = this
         lower.upper = this

      def seqHorizontal(): Seq[Node] = Iterator.iterate(right)(_.right).takeWhile(_ != this).toSeq

      def seqVertical(): Seq[Node] = Iterator.iterate(lower)(_.lower).takeWhile(_ != this).toSeq

      def cover(): Unit

      def uncover(): Unit

   /**
    * @param constraint which constraint it is.
    * @param covered    if the column is covered.
    */
   case class Constraint(constraint: Int, var covered: Boolean = false) extends Node():

      def linkLowest[N <: Node](insertNode: N): N = upper.linkLower(insertNode)

      override def cover(): Unit =
         covered = true
         removeHorizontal()
         getColumn.foreach(cn => cn.seqHorizontal().foreach(_.removeVertical()))

      override def uncover(): Unit =
         covered = false
         getColumn.foreach(cn => cn.seqHorizontal().foreach(_.reinsertVertical()))
         reinsertHorizontal();

      def getColumn: Seq[LinkedNode] = this.seqVertical().map(_.asInstanceOf[LinkedNode])

   case class LinkedNode(constraint: Constraint, field: (Int, Int, Int)) extends Node():
      constraint.linkLowest(this)

      override def cover(): Unit = this.seqHorizontal().foreach(_.asInstanceOf[LinkedNode].constraint.cover())

      override def uncover(): Unit = this.seqHorizontal().foreach(_.asInstanceOf[LinkedNode].constraint.uncover())
