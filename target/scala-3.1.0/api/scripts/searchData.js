pages = [{"l":"index.html","n":"SudokuSolver","t":"SudokuSolver","d":"","k":"static"},
{"l":"_empty_.html","n":"<empty>","t":"package <empty>","d":"<empty>","k":"package"},
{"l":"_empty_/Main$.html","n":"Main","t":"object Main extends App","d":"<empty>/Main$","k":"object"},
{"l":"_empty_/Main$.html","n":"duration","t":"val duration: Double","d":"<empty>/Main$","k":"val"},
{"l":"_empty_/Main$.html","n":"ecs","t":"val ecs: String","d":"<empty>/Main$","k":"val"},
{"l":"_empty_/Main$.html","n":"lgs","t":"val lgs: String","d":"<empty>/Main$","k":"val"},
{"l":"_empty_/Main$.html","n":"solved","t":"val solved: Sudoku","d":"<empty>/Main$","k":"val"},
{"l":"_empty_/Main$.html","n":"sudokuAntiBruteForce","t":"val sudokuAntiBruteForce: Sudoku","d":"<empty>/Main$","k":"val"},
{"l":"_empty_/Main$.html","n":"sudokuEasy","t":"val sudokuEasy: Sudoku","d":"<empty>/Main$","k":"val"},
{"l":"_empty_/Main$.html","n":"sudokuHard","t":"val sudokuHard: Sudoku","d":"<empty>/Main$","k":"val"},
{"l":"_empty_/Main$.html","n":"sudokuHell","t":"val sudokuHell: Sudoku","d":"<empty>/Main$","k":"val"},
{"l":"_empty_/Main$.html","n":"sudokuMedium","t":"val sudokuMedium: Sudoku","d":"<empty>/Main$","k":"val"},
{"l":"_empty_/Main$.html","n":"sudokuZeros","t":"val sudokuZeros: Sudoku","d":"<empty>/Main$","k":"val"},
{"l":"_empty_/Main$.html","n":"t1","t":"val t1: Long","d":"<empty>/Main$","k":"val"},
{"l":"solver.html","n":"solver","t":"package solver","d":"solver","k":"package"},
{"l":"solver/Backtracking.html","n":"Backtracking","t":"class Backtracking(sudoku: Sudoku) extends Solver","d":"solver/Backtracking","k":"class"},
{"l":"solver/DancingLinks.html","n":"DancingLinks","t":"class DancingLinks(sudoku: Sudoku) extends Solver","d":"solver/DancingLinks","k":"class"},
{"l":"solver/DancingLinks.html","n":"grid","t":"val grid: Seq[Constraint]","d":"solver/DancingLinks","k":"val"},
{"l":"solver/DancingLinks.html","n":"linkedGridString","t":"def linkedGridString: String","d":"solver/DancingLinks","k":"def"},
{"l":"solver/DancingLinks$Constraint.html","n":"Constraint","t":"class Constraint(constraint: Int, var covered: Boolean) extends Node","d":"solver/DancingLinks$Constraint","k":"class"},
{"l":"solver/DancingLinks$Constraint.html","n":"cover","t":"def cover(): Unit","d":"solver/DancingLinks$Constraint","k":"def"},
{"l":"solver/DancingLinks$Constraint.html","n":"getColumn","t":"def getColumn: Seq[LinkedNode]","d":"solver/DancingLinks$Constraint","k":"def"},
{"l":"solver/DancingLinks$Constraint.html","n":"linkLowest","t":"def linkLowest[N <: Node](insertNode: N): N","d":"solver/DancingLinks$Constraint","k":"def"},
{"l":"solver/DancingLinks$Constraint.html","n":"uncover","t":"def uncover(): Unit","d":"solver/DancingLinks$Constraint","k":"def"},
{"l":"solver/DancingLinks$LinkedNode.html","n":"LinkedNode","t":"class LinkedNode(constraint: Constraint, field: (Int, Int, Int)) extends Node","d":"solver/DancingLinks$LinkedNode","k":"class"},
{"l":"solver/DancingLinks$Node.html","n":"Node","t":"trait Node()","d":"solver/DancingLinks$Node","k":"trait"},
{"l":"solver/DancingLinks$Node.html","n":"left","t":"var left: Node","d":"solver/DancingLinks$Node","k":"var"},
{"l":"solver/DancingLinks$Node.html","n":"linkLower","t":"def linkLower[N <: Node](insertNode: N): N","d":"solver/DancingLinks$Node","k":"def"},
{"l":"solver/DancingLinks$Node.html","n":"linkRight","t":"def linkRight[N <: Node](insertNode: N): N","d":"solver/DancingLinks$Node","k":"def"},
{"l":"solver/DancingLinks$Node.html","n":"lower","t":"var lower: Node","d":"solver/DancingLinks$Node","k":"var"},
{"l":"solver/DancingLinks$Node.html","n":"reinsertHorizontal","t":"def reinsertHorizontal(): Unit","d":"solver/DancingLinks$Node","k":"def"},
{"l":"solver/DancingLinks$Node.html","n":"reinsertVertical","t":"def reinsertVertical(): Unit","d":"solver/DancingLinks$Node","k":"def"},
{"l":"solver/DancingLinks$Node.html","n":"removeHorizontal","t":"def removeHorizontal(): Unit","d":"solver/DancingLinks$Node","k":"def"},
{"l":"solver/DancingLinks$Node.html","n":"removeVertical","t":"def removeVertical(): Unit","d":"solver/DancingLinks$Node","k":"def"},
{"l":"solver/DancingLinks$Node.html","n":"right","t":"var right: Node","d":"solver/DancingLinks$Node","k":"var"},
{"l":"solver/DancingLinks$Node.html","n":"seqSideways","t":"def seqSideways(): Seq[Node]","d":"solver/DancingLinks$Node","k":"def"},
{"l":"solver/DancingLinks$Node.html","n":"seqVertical","t":"def seqVertical(): Seq[Node]","d":"solver/DancingLinks$Node","k":"def"},
{"l":"solver/DancingLinks$Node.html","n":"upper","t":"var upper: Node","d":"solver/DancingLinks$Node","k":"var"},
{"l":"solver/ExactCover.html","n":"ExactCover","t":"class ExactCover(sudoku: Sudoku) extends Solver","d":"solver/ExactCover","k":"class"},
{"l":"solver/ExactCover.html","n":"getExactCoverString","t":"def getExactCoverString: String","d":"solver/ExactCover","k":"def"},
{"l":"solver/Solver.html","n":"Solver","t":"trait Solver(sudoku: Sudoku)","d":"solver/Solver","k":"trait"},
{"l":"solver/Solver.html","n":"solve","t":"def solve: Sudoku","d":"solver/Solver","k":"def"},
{"l":"sudoku.html","n":"sudoku","t":"package sudoku","d":"sudoku","k":"package"},
{"l":"sudoku/Sudoku.html","n":"Sudoku","t":"class Sudoku(sudoku: Array[Int])","d":"sudoku/Sudoku","k":"class"},
{"l":"sudoku/Sudoku.html","n":"getBox","t":"def getBox(box: Int): Iterator[Int]","d":"sudoku/Sudoku","k":"def"},
{"l":"sudoku/Sudoku.html","n":"getField","t":"def getField(row: Int, column: Int): Int","d":"sudoku/Sudoku","k":"def"},
{"l":"sudoku/Sudoku.html","n":"getField","t":"def getField(field: Int): Int","d":"sudoku/Sudoku","k":"def"},
{"l":"sudoku/Sudoku.html","n":"getGrid","t":"def getGrid: Array[Int]","d":"sudoku/Sudoku","k":"def"},
{"l":"sudoku/Sudoku.html","n":"getNumberedGrid","t":"def getNumberedGrid: Array[(Int, (Int, Int, Int))]","d":"sudoku/Sudoku","k":"def"},
{"l":"sudoku/Sudoku.html","n":"isBoxValid","t":"def isBoxValid(box: Int): Boolean","d":"sudoku/Sudoku","k":"def"},
{"l":"sudoku/Sudoku.html","n":"isColumnValid","t":"def isColumnValid(column: Int): Boolean","d":"sudoku/Sudoku","k":"def"},
{"l":"sudoku/Sudoku.html","n":"isRowValid","t":"def isRowValid(row: Int): Boolean","d":"sudoku/Sudoku","k":"def"},
{"l":"sudoku/Sudoku.html","n":"isValid","t":"def isValid: Boolean","d":"sudoku/Sudoku","k":"def"},
{"l":"sudoku/Sudoku.html","n":"randomNextField","t":"def randomNextField: Int","d":"sudoku/Sudoku","k":"def"},
{"l":"sudoku/Sudoku.html","n":"this","t":"def this(string: String)","d":"sudoku/Sudoku","k":"def"},
{"l":"sudoku/Sudoku.html","n":"update","t":"def update(field: Int, value: Int): Sudoku","d":"sudoku/Sudoku","k":"def"},
{"l":"sudoku/Sudoku$.html","n":"Sudoku","t":"object Sudoku","d":"sudoku/Sudoku$","k":"object"},
{"l":"sudoku/Sudoku$.html","n":"fieldToCords","t":"def fieldToCords(field: Int): (Int, Int, Int)","d":"sudoku/Sudoku$","k":"def"},
{"l":"sudoku/Sudoku$.html","n":"whichBox","t":"def whichBox(field: Int): Int","d":"sudoku/Sudoku$","k":"def"},
{"l":"docs/index.html","n":"SudokuSolver","t":"SudokuSolver","d":"","k":"static"}];