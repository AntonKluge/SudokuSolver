package solver

import sudoku.Sudoku

class DancingLinks(sudoku: Sudoku) extends Solver(sudoku):

  trait Node(var left: Node, var right: Node, var upper: Node, var lower: Node):
    left = this; right = this; upper = this; lower = this

    def linkRight(insertNode: Node): Node =
      insertNode.right = right
      insertNode.right.left = insertNode
      insertNode.left = this
      right = insertNode
      insertNode

    def linkLower(insertNode: Node): Node =
      insertNode.lower = lower
      insertNode.lower.upper = insertNode
      insertNode.upper = this
      lower = insertNode
      insertNode

  case class Constraint(constraint: Int) extends Node(null, null, null, null):

    def linkLowest(insertNode: Node): Node = upper.linkLower(insertNode)

  case class Possibility(possibility: Int) extends Node(null, null, null, null):

    def linkRightest(insertNode: Node): Node = left.linkRight(insertNode)

  case class LinkedNode() extends Node(null, null, null, null)

  override def solve: Sudoku =
    val grid = linkedListGrid()


    null


  private def linkedListGrid(): (Seq[Possibility], Seq[Constraint]) =
    val grid = (Seq.range(0, 729).map(Possibility.apply), Seq.range(0, 324).map(Constraint.apply))
    sudoku.getGrid.zip((0 to 8).flatMap(r => (0 to 8).map(c => (r, c)))).foreach { field =>
      val (d, (r, c)) = field
      val b = Sudoku.whichBox(r * 9 + c)
      // colum row existing constraint
      if d != 0 then grid._2(r * 9 + c).linkLowest(grid._1(d - 1).linkRightest(LinkedNode()))
      else (0 to 8).foreach(nd => grid._2(r * 9 + c).linkLowest(grid._1(nd).linkRightest(LinkedNode())))
      Seq((81, r), (162, c), (243, b)).foreach { t => 
        if d != 0 then grid._2(t._1 + t._2 * 9 + (d - 1)).linkLowest(grid._1(d - 1).linkRightest(LinkedNode()))
        else (0 to 8).foreach(nd => grid._2(t._1 + t._2 * 9 + nd).linkLowest(grid._1(nd).linkRightest(LinkedNode())))
      }
    }
    grid