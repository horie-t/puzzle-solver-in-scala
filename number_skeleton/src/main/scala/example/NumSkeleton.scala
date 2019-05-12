package example

/** ナンバー・スケルトンを解く
  */
object NumSkelton {
  /** 問題を解きます。
    * @param board 問題のボード。"."で空いているマスを、"*"で埋める必要のないマスを表します。
    * @param nums 埋める数字。keyは文字数、valueは、数字のリストのリスト
    */
  def solve(board: Array[Array[Char]], nums: Map[Int, Array[Array[Char]]]): Array[Array[Char]] = {
    board
  }
}

object NumSkeletonApp {
  import NumSkelton._

  def main(args: Array[String]) {
    val board = Array("....*", ".**.*", "..*..", "*.**.", "*....").map(_.toCharArray())
    val nums = Map(
      (4 -> Array("1766", "7466").map(_.toCharArray())),
      (3 -> Array("176", "646", "711", "744").map(_.toCharArray())),
      (2 -> Array("47", "61").map(_.toCharArray()))
    )
    solve(board, nums).map(raw => println(raw.mkString))
  }
}
