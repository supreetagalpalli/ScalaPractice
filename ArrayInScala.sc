import scala.collection.mutable.{ArrayBuffer, ListBuffer}
/* Given an array a that contains only numbers in the range from 1 to a.length, 
find the first duplicate number for which the second occurrence has the minimal
index. In other words, if there are more than 1 duplicated numbers, return the 
number for which the second occurrence has a smaller index than the second 
occurrence of the other number does. If there are no such elements, return -1.
*/

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
/* Given a string s consisting of small English letters, find and return the first 
instance of a non-repeating character in it. If there is no such character, return '_'.
*/

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

/* You are given an n x n 2D matrix that represents an image. 
Rotate the image by 90 degrees (clockwise).
*/
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

/* Implement an algorithm that will check whether the given grid of numbers 
represents a valid Sudoku puzzle according to the layout rules described above. 
Note that the puzzle represented by grid does not have to be solvable.
*/

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

/*You have an array of strings crypt, the cryptarithm, and an an array 
containing the mapping of letters and digits, solution. The array crypt 
will contain three non-empty strings that follow the structure: 
[word1, word2, word3], which should be interpreted as the word1 + word2 = word3 cryptarithm.

If crypt, when it is decoded by replacing all of the letters in the cryptarithm
with digits using the mapping in solution, becomes a valid arithmetic equation 
containing no numbers with leading zeroes, the answer is true. If it does not 
become a valid arithmetic solution, the answer is false.
*/

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
