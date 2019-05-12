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
    val board = """|....*
                   |.**.*
                   |..*..
                   |*.**.
                   |*....""".stripMargin.split("\n").map(_.toCharArray())
    val bones = Array(
      Bone(Point(0, 0), 4, Col),
      Bone(Point(4, 1), 4, Col),
      Bone(Point(0, 0), 3, Row),
      Bone(Point(0, 3), 3, Row),
      Bone(Point(2, 1), 3, Row),
      Bone(Point(2, 4), 3, Row),
      Bone(Point(2, 0), 2, Col),
      Bone(Point(2, 3), 2, Col)
    )
    val nums = Map(
      (4 -> Array("1766", "7466").map(_.toCharArray())),
      (3 -> Array("176", "646", "711", "744").map(_.toCharArray())),
      (2 -> Array("47", "61").map(_.toCharArray()))
    )
    solve(board, bones, nums) match {
      case None => println("Fail to solve.")
      case Some(b) => b.map(raw => println(raw.mkString))
    }
  }
}
