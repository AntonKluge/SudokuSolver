package sudoku

case class Sudoku(sudoku: Array[Int]):

  def this(string: String) =
    this(string.linesIterator.flatMap(_.split("").map(_.toInt)).toArray)

  def getField(row: Int, column: Int): Int = sudoku(row * 9 + column)

  def getField(field: Int): Int = sudoku(field)

  def isRowValid(row: Int): Boolean =
    validCollection(sudoku.slice(row * 9, row * 9 + 9).toIterator)

  def isColumnValid(column: Int): Boolean =
    validCollection(sudoku.drop(column).sliding(1, 9).map(_(0)))

  def isBoxValid(box: Int): Boolean = validCollection(this.getBox(box))

  def getBox(box: Int): Iterator[Int] = (box match
    case 0 | 1 | 2 => sudoku.drop(box * 3)
    case 3 | 4 | 5 => sudoku.drop(27 + (box - 3) * 3)
    case 6 | 7 | 8 => sudoku.drop(54 + (box - 6) * 3)
  ).sliding(3, 9).take(3).flatten

  def isValid: Boolean = (0 to 8).forall(n => isRowValid(n) && isColumnValid(n) && isBoxValid(n))

  def update(field: Int, value: Int): Sudoku = 
    sudoku.update(field, value)
    this
    
  def getGrid: Array[Int] = sudoku  

  override def toString: String = sudoku.sliding(9, 9).map(_.mkString(" | ")).mkString("\n")

  private def validCollection(partSudoku: Iterator[Int]): Boolean =
    val mSet = scala.collection.mutable.Set[Int]()
    partSudoku.forall(x => if x == 0 then true else if mSet(x) then false else {mSet += x; true})

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