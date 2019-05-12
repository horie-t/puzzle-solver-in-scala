package example

sealed trait Direction
case object Row extends Direction
case object Col extends Direction

case class Point(row: Int, col: Int)
case class Bone(p: Point, length: Int, dir: Direction)

/** ナンバー・スケルトンを解く
  */
object NumSkelton {
  type Board = Array[Array[Char]]

  /** 問題を解きます。
    * @param board 問題の盤面。"."で空いているマスを、"*"で埋める必要のないマスを表します。
    * @param bones 空白データ
    * @param nums 埋める数字のMap。keyは文字数、valueは数字のリストのリスト
    */
  def solve(board: Board, bones: Array[Bone], nums: Map[Int, Array[Array[Char]]]): Option[Board] = {
    if (bones.isEmpty) {
      // 全て割当てた。
      Some(board)
    } else {
      for (bone <- bones) {
        for (num <- nums(bone.length)) {
          val assignedBoard = assign(board, bone, num) match {
            case None => None
            case Some(newBoard) => {
              // 割当てに成功したら、残りを埋めていく
              val newBones = bones.filter(_ != bone)
              val newNums = nums.updated(bone.length, nums(bone.length).filter(_ != num))
              solve(newBoard, newBones, newNums)
            }
          }
          if (!assignedBoard.isEmpty) return assignedBoard;
        }
        return None
      }
      None
    }
  }

  /** 盤面に数字を割り当てた結果を返します。割当てできない場合はNoneを返します。
    */
  def assign(board: Board, bone: Bone, num: Array[Char]) : Option[Board] = {
    val newBoard = board.map(_.clone())
    for (i <- 0 until bone.length) {
      val r = if (bone.dir == Col) bone.p.row     else bone.p.row + i
      val c = if (bone.dir == Col) bone.p.col + i else bone.p.col
      if (newBoard(r)(c) == '.') {
        newBoard(r)(c) = num(i)
      } else if (newBoard(r)(c) != num(i)) {
        return None
      }
    }
    Some(newBoard)
  }
}

object NumSkeletonApp {
  import NumSkelton._

  def main(args: Array[String]) {
    val board = """|.....*...
                   |.***...*.
                   |.....*.*.
                   |*.*.*....
                   |*.*...*.*
                   |....*.*.*
                   |.*.*.....
                   |.*...***.
                   |...*.....""".stripMargin.split("\n").map(_.toCharArray())
    val bones = calcBones(board)
    val nums = Map(
      (5 -> Array("11994", "40449", "49114", "94491").map(_.toCharArray())),
      (4 -> Array("1911", "1944", "2019", "2020", "2510", "4194", "4491", "5439", "9134", "9944").map(_.toCharArray())),
      (3 -> Array("154", "255", "350", "451", "454", "455", "459", "555", "951").map(_.toCharArray()))
    )
    solve(board, bones, nums) match {
      case None => println("Fail to solve.")
      case Some(b) => b.map(raw => println(raw.mkString))
    }
  }

  def calcBones(board: Board) : Array[Bone] = {
    var bones = Array[Bone]()
    val width = board(0).size; val height = board.size
    for (r <- 0 until height) {
      for (c <- 0 until width) {
        bones = bones ++ getBones(board, r, c)
      }
    }
    bones
  }

  def getBones(board: Board, row: Int, col: Int): Array[Bone] = {
    getBone(board, row, col, Col) ++ getBone(board, row, col, Row)
  }

  def getBone(board: Board, row: Int, col: Int, dir: Direction): Array[Bone] = {
    if (board(row)(col) == '*') return Array[Bone]()

    val width = board(0).size; val height = board.size
    if (dir == Col) {
      if (col != 0 && board(row)(col - 1) == '.'
        || col == width - 1
        || board(row)(col + 1) == '*') {
        Array()
      } else {
        for (i <- 1 until width - col) {
          if (board(row)(col + i) == '*') {
            return Array(Bone(Point(row, col), i, Col))
          }
        }
        Array(Bone(Point(row, col), width - col, Col))
      }
    } else {
      if (row != 0 && board(row - 1)(col) == '.'
        || row == height - 1
        || board(row + 1)(col) == '*') {
        Array()
      } else {
        for (i <- 1 until height - row) {
          if (board(row + i)(col) == '*') {
            return Array(Bone(Point(row, col), i, Row))
          }
        }
        Array(Bone(Point(row, col), height - row, Row))
      }
    }
  }
}
