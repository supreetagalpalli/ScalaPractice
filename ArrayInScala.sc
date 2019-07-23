import scala.collection.mutable.{ArrayBuffer, ListBuffer}

def firstDuplicate(a: Array[Int]): Int =  {
  var seen: Set[Int] = Set.empty[Int]
  var res = -1
  for(elem <- a){
    if(seen.contains(elem)) {
      res = elem
      return res
    }
    else
      seen += elem
  }

  res
}

firstDuplicate(Array(1,2,3,4,3,2))

def firstNotRepeatingCharacter(s: String): Char = {
  var repeating = Set[Char]()
  var nonrepeating = ListBuffer[Char]()

  for(c <- s){
    if(nonrepeating.contains(c)){
      nonrepeating -= c
      repeating += c
    }
    if (!repeating.contains(c))
      nonrepeating += c
  }
  if(nonrepeating.isEmpty) '_' else nonrepeating(0)
}

firstNotRepeatingCharacter("abcabdc")

def rotateImage(a: Array[Array[Int]]): Array[Array[Int]] = {
  var n = a.length - 1
  var row = a.length
  var col =a(0).length
  var b = Array.fill(row,col)(0)
  for(i <- 0 to n) {
    for (j <- 0 to n) {
      b(j)(n - i) = a(i)(j)
    }
  }
  b
}

rotateImage(Array(Array(1,2,3),Array(4,5,6), Array(7,8,9)))

def rows(grid: Array[Array[Char]]): Boolean = {
  var bool = true
  var row = grid.length
  for (i <- 0 until row) {
    var t = grid(i).filterNot(_ == '.')
    if (t.distinct.size != t.size)
      bool = false
  }
  bool
}

def matrix(grid: Array[Array[Char]]): Boolean = {
  val squareSize = 3
  val boardSize = grid.length

  val squareLines = for {
    rowStart <- List.range(0, boardSize, squareSize)
    colStart <- List.range(0, boardSize, squareSize)
  } yield {
    List.range(0, squareSize).flatMap(i => grid(rowStart + i).slice(colStart, colStart + squareSize))
  }.toArray
  rows(squareLines.toArray)
}

def sudoku2(grid: Array[Array[Char]]): Boolean = {
  if(rows(grid) == true && rows(grid.transpose) == true && matrix(grid) == true)
    true
  else false
}

val grid = Array(
  Array('.','.','.','.','.','.','.','.','2'),
  Array('.','.','.','.','.','.','6','.','.'),
  Array('.','.','1','4','.','.','8','.','.'),
  Array('.','.','.','.','.','.','.','.','.'),
  Array('.','.','.','.','.','.','.','.','.'),
  Array('.','.','.','.','3','.','.','.','.'),
  Array('5','.','8','6','.','.','.','.','.'),
  Array('.','9','.','.','.','.','4','.','.'),
  Array('5','.','.','.','5','.','.','.','.'),
)
sudoku2(grid)

def isCryptSolution(crypt: Array[String], solution: Array[Array[Char]]): Boolean = {
  var temp = ArrayBuffer[String]()
  var newMap = Map[Char,Char]()

  newMap = solution.map(t => (t(0) -> t (1))).toMap

  for(elem <- crypt){
    var add =  ""
    for(c <- elem){
      add = add.concat(newMap.get(c).get.toString)
    }
    temp += add
  }

  if(temp(0)(0).toString.toInt == 0 && temp(0).toString.length != 1)
    return false

  if(temp(1)(0).toString.toInt == 0 && temp(1).toString.length != 1)
    return false

  if(temp(2)(0).toString.toInt == 0 && temp(1).toString.length != 1)
    return false

  temp(0).toString.toLong + temp(1).toString.toLong == temp(2).toString.toLong
}

val sol = Array(Array('O','0'),
  Array('M','1'),
  Array('Y','2'),
  Array('E','5'),
  Array('N','6'),
  Array('D','7'),
  Array('R','8'),
  Array('S','9'))

isCryptSolution(Array("SEND", "MORE", "MONEY"),sol)