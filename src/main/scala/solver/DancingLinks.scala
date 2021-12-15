package solver

import sudoku.Sudoku

/**
 * A algorithm to solve sudokus by converting them into exact cover problems and then
 * using algorithm Xto solve them. The basics are a 729 x 324 grid of one and zeros
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

   lazy val grid: Seq[Constraint] = linkedListGrid()

   /**
    *
    */
   trait Node():

      var left, right, upper, lower = this

      /**
       *
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
       *
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

      def seqVertical(): Seq[Node] =Iterator.iterate(lower)(_.lower).takeWhile(_ != this).toSeq


   /**
    *
    * @param constraint which constraint it is.
    * @param covered    if the column is covered.
    */
   case class Constraint(constraint: Int, var covered: Boolean = false) extends Node() :

      def linkLowest[N <: Node](insertNode: N): N = upper.linkLower(insertNode)

      def getColumn: Seq[LinkedNode] = this.seqVertical().map(_.asInstanceOf[LinkedNode])

      def cover(): Unit =
         covered = true
         removeHorizontal()
         getColumn.foreach(cn => cn.seqHorizontal().foreach(_.removeVertical()))

      def uncover(): Unit =
         covered = false
         getColumn.foreach(cn => cn.seqHorizontal().foreach(_.reinsertVertical()))
         reinsertHorizontal();

   case class LinkedNode(constraint: Constraint, field: (Int, Int, Int)) extends Node() :
      constraint.linkLowest(this)

   /**
    *
    * @return
    */
   override def solve: Sudoku = updateSudoku(algorithmX().getOrElse(Seq.empty))


   /**
    *
    * @param results the accumulated results, in the beginning start with a empty Seq.
    * @return
    */
   private def algorithmX(results: Seq[LinkedNode] = Seq.empty[LinkedNode]): Option[Seq[LinkedNode]] =
      if grid.forall(_.covered) then Some(results)
      else
         val chosenCol = chooseColumn()
         chosenCol.cover()

         chosenCol.getColumn.foreach { node =>
            node.seqHorizontal().foreach(_.asInstanceOf[LinkedNode].constraint.cover())
            val res = algorithmX(results :+ node)
            if res.isDefined then return res
            node.seqHorizontal().foreach(_.asInstanceOf[LinkedNode].constraint.uncover())
         }
         chosenCol.uncover()
         None

   /**
    *
    * @return
    */
   private def chooseColumn(): Constraint = grid.filter(!_.covered).minBy(_.seqVertical().size)

   /**
    *
    * @return
    */
   private def linkedListGrid(): Seq[Constraint] =
      val grid = Seq.iterate((Constraint(0), 1), 324)(t => // constraints for 0 to 324
         (t._1.linkRight(Constraint(t._2)), t._2 + 1)).map(_._1) // linked to each other
      sudoku.getNumberedGrid.foreach { field =>
         val (d, (r, c, b)) = field // match the digit, row, column and box out of field
         val calcPos = Seq((i: Int) => r * 9 + c, // at least one in every box
            i => 81 + r * 9 + i, // exactly one of each in every row
            i => 162 + c * 9 + i, // exactly one of each in every column
            i => 243 + b * 9 + i) // exactly one of each in every box
         (if d == 0 then 0 to 8 else Seq(d - 1)) // if theres a clue, we only create it, otherwise 0 to 8 possibilities
            .foreach(nd => calcPos.map(f => LinkedNode(grid(f(nd)), (r, c, nd + 1)))
               .reduceLeft((insTo, newNode) => insTo.linkRight(newNode))) // create them and link to each other (and up)
      }
      grid

   private def updateSudoku(res: Seq[LinkedNode]): Sudoku =
      res.foreach(node => sudoku.update(node.field._1 * 9 + node.field._2, node.field._3))
      sudoku
