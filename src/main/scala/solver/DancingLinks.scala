package solver

import sudoku.Sudoku

/**
 *
 * @param sudoku : The sudoku to solve
 */
class DancingLinks(sudoku: Sudoku) extends Solver(sudoku) :

   /**
    *
    */
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

      def reinsertVertical(): Unit =
         upper.lower = this
         lower.upper = this

      def reinsertHorizontal(): Unit =
         left.right = this
         right.left = this

      def seqSideways(): Seq[Node] = Iterator.iterate(right)(_.right).takeWhile(_ != this).toSeq

      def seqVertical(): Seq[Node] = Iterator.iterate(lower)(_.lower).takeWhile(_ != this).toSeq

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
         getColumn.foreach(cn => cn.seqVertical().drop(1).foreach(_.removeVertical()))

      def uncover(): Unit =
         covered = false
         this.getColumn.reverse.foreach(cn => cn.seqVertical().drop(1).foreach(_.reinsertVertical()))
         reinsertHorizontal();

   case class LinkedNode(constraint: Constraint, field: (Int, Int, Int)) extends Node() :
      constraint.linkLowest(this)

   /**
    *
    * @return
    */
   override def solve: Sudoku = null //TODO

   /**
    *
    * @param results the accumulated results, in the beginning start with a empty Seq.
    * @return
    */
   private def algorithmX(results: Seq[Constraint] = Seq.empty[Constraint]): Option[Seq[Constraint]] = //TODO
      if grid.forall(_.covered) then Some(results)
      else
         val chosenCol = chooseColumn()
         chosenCol.cover()
         chosenCol.getColumn.foreach(node => null)
         null

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
            .foreach(nd => calcPos.map(f => LinkedNode(grid(f(nd)), (81 * r + 9 * c + nd, f(nd), nd + 1)))
               .reduceLeft((insTo, newNode) => insTo.linkRight(newNode))) // create them and link to each other (and up)
      }
      grid

   /**
    *
    * @return
    */
   def linkedGridString: String =
      val arr = Array.ofDim[Int](729, 324)
      grid.foreach(con => con.getColumn.foreach(n => arr(n.field._1)(n.field._2) = 1))
      arr.map(_.map(i => if i == 0 then " " else "X").mkString).mkString("\n")
