package example

sealed trait Direction
case object Raw extends Direction
case object Col extends Direction

case class Point(row: Int, col: Int)
case class Bone(p: Point, length: Int, dir: Direction)

/** ナンバー・スケルトンを解く
  */
object NumSkelton {
  /** 問題を解きます。
    * @param board 問題のボード。"."で空いているマスを、"*"で埋める必要のないマスを表します。
    * @param bones 空白データ
    * @param nums 埋める数字のMap。keyは文字数、valueは数字のリストのリスト
    */
  def solve(board: Array[Array[Char]], bones: Array[Bone],
    nums: Map[Int, Array[Array[Char]]]): Option[Array[Array[Char]]] = {
    if (bones.isEmpty) {
      Some(board)
    } else {
      for (bone <- bones) {
        val newBones = bones.filter(_ != bone)
        solve(board, newBones, nums)
      }
      Some(board)
    }
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
      Bone(Point(0, 0), 4, Raw),
      Bone(Point(4, 1), 4, Raw),
      Bone(Point(0, 0), 3, Col),
      Bone(Point(0, 3), 3, Col),
      Bone(Point(2, 1), 3, Col),
      Bone(Point(2, 4), 3, Col),
      Bone(Point(2, 0), 2, Raw),
      Bone(Point(2, 3), 2, Raw)
    )
    val nums = Map(
      (4 -> Array("1766", "7466").map(_.toCharArray())),
      (3 -> Array("176", "646", "711", "744").map(_.toCharArray())),
      (2 -> Array("47", "61").map(_.toCharArray()))
    )
    solve(board, bones, nums) match {
      case None => println("Fail to solve.")
      case Some(board) => board.map(raw => println(raw.mkString))
    }
  }
}
