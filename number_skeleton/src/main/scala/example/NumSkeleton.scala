package example

sealed trait Direction
case object Raw extends Direction
case object Col extends Direction

case class Bone(row: Int, col: Int, dir: Direction)

/** ナンバー・スケルトンを解く
  */
object NumSkelton {
  /** 問題を解きます。
    * @param board 問題のボード。"."で空いているマスを、"*"で埋める必要のないマスを表します。
    * @param bones 空白データのMap。keyは文字数、valueは空白データのリスト
    * @param nums 埋める数字のMap。keyは文字数、valueは数字のリストのリスト
    */
  def solve(board: Array[Array[Char]], bones: Map[Int, Array[Bone]],
    nums: Map[Int, Array[Array[Char]]]): Option[Array[Array[Char]]] = {
    Some(board)
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
    val bones = Map(
      (4 -> Array(Bone(0, 0, Raw), Bone(4, 1, Raw))),
      (3 -> Array(Bone(0, 0, Col), Bone(0, 3, Col), Bone(2, 1, Col), Bone(2, 4, Col))),
      (2 -> Array(Bone(2, 0, Raw), Bone(2, 3, Raw)))
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
