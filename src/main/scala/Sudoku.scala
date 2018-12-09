/* 『あらゆる数独パズルを解く』(Peter Norvig) のScala版です。
 * 解説は以下を参照
 * http://www.aoky.net/articles/peter_norvig/sudoku.htm
 */

/** 数独を解くオブジェクトです。
  */
object Sudoku {
  /** 行名、列名を組み合わせて、セル名を作成します。
    * 例: Seq("A1", "A2", "A3", ..., "A9")
    * 
    * @param rows 行名を並べた文字列。例: "ABC...I"
    * @param cols 列名を並べた文字列。例: "123...9"
    * @return セル名の並び
    */
  def cross(rows: String,  cols: String): Seq[String] =
    for (r <- rows.split(""); c <- cols.split("")) yield (r + c)

  val digits: String = "123456789" // 数独は1〜9の数字を使う
  val rows: String   = "ABCDEFGHI" // 行の名前はアルファベットで表記
  val cols: String   = digits      // 列の名前は数字で表記
  val cellNames: Seq[String] = cross(rows, cols) // 全てのセル名の並び
  val allUnits: Seq[Seq[String]] =               // 全てのUnitの並び
    cols.map(c => cross(rows, c.toString)) ++
    rows.map(r => cross(r.toString, cols)) ++
    (for (rs <- rows.grouped(3); cs <- cols.grouped(3)) yield (cross(rs, cs)))
  // セルと、セルを含むUnitリストの対応
  val unitMap: Map[String, Seq[Seq[String]]] = Map({ 
    for {cellName <- cellNames
    } yield {(cellName, allUnits.filter(_.contains(cellName)))}
  }: _*)
  // セルと、そのセルのPeerの対応
  val peerMap: Map[String, Set[String]] = Map({ //
    for {cellName <- cellNames
    } yield {(cellName, Set(unitMap.getOrElse(cellName, Nil).flatten: _*) - cellName)}
  }: _*)

  /** 盤面の入力文字列をパースし、盤面(grid)を返します。
    * @param gridStr 盤面の文字列。盤面の数字を行優先で並べた文字列。空白は「.」か「0」の文字にする事。改行や区切り文字もOK
    * @return 盤面。セル名と、そのセルに入る可能性のある数字の文字列のマップ
    */
  def parseGrid(gridStr: String): Map[String, String] = {
    /** 盤面に不要な文字を削除し、入力された盤面を返します。
      */
    def gridValues(grid: String): Map[String, String] = {
      val chars = grid.filter { c => digits.contains(c) || "0.".contains(c)}
      if (chars.length != 81) throw new IllegalArgumentException("Invalid grid data: " + grid)
      Map(cellNames zip chars.split(""): _*)
    }

     // 初期盤面(全てのセルに、0〜9までの数字が入る可能性がある)
    var grid: Map[String, String] =
      Map({for (cellName <- cellNames) yield ((cellName, digits))}: _*)

    for ((cellName, digit) <- gridValues(gridStr))
      if (digits.contains(digit))
        assign(grid, cellName, digit) match {
          case None => throw new IllegalArgumentException("Illegal Grid Data.")
          case Some(assignedGrid) => grid = assignedGrid
        }

    grid
  }

  /** 指定されたマス目に指定された数字を割り当てます。
    * 他の可能性のあった値を取り除きながら、制約を伝搬させる。
    * @param grid 盤面
    * @param cellName マス目の名前
    * @param digit マス目に割り当てる数字
    * 
    * @return 割り当てた盤面。割り当てに矛盾が発生したらNone。
    */
  def assign(grid: Map[String, String], cellName: String, digit: String): Option[Map[String, String]] = {
    val othereValues = grid.getOrElse(cellName, "").replace(digit, "") // 割り当てられる可能性のあった他の数字
    var updateGrid = grid

    // 他の値の全てについてこのセルから取り除きながら、制約を伝搬させる
    for (d2 <- othereValues.split(""))
      eliminate(updateGrid, cellName, d2) match {
        case None => return None
        case Some(eliminatedGrid) => updateGrid = eliminatedGrid
      }

    Some(updateGrid)
  }

  /** マス目から指定された数字を取り除きます。この時、制約を再帰的に伝搬させる。
    * @param grid 盤面
    * @param cellName マス目の名前
    * @param digit 取り除く数字
    * 
    * @return 取り除いた盤面。割り当てに矛盾が発生したらNone。
    */
  def eliminate(grid: Map[String, String], cellName: String, digit: String): Option[Map[String, String]] = {
    var updateGrid = Map.empty[String, String]

    // マス目から指定された数字を取り除く
    grid.get(cellName) match {
      case None => return throw new IllegalStateException("eliminate: Invalid cellName: " + cellName)
      case Some(cellVal) if (!cellVal.contains(digit)) =>
        return Some(grid) // 既に値が取り除かれているのでそのまま返す。
      case Some(cellVal)  => {
        val eliminatedCellVal = cellVal.replace(digit, "")
        if (eliminatedCellVal.length == 0) {
          return None // セルの値がなくなってしまったので矛盾
        } else {
          updateGrid = grid.updated(cellName, eliminatedCellVal)
          if (eliminatedCellVal.length == 1) 
            // マス目の数字が一つになったら、ピアのセルから、その数字を取り除く
            for (peerCellName <- peerMap.getOrElse(cellName, Seq.empty))
              eliminate(updateGrid, peerCellName, eliminatedCellVal) match {
                case None => return None // 矛盾が発生
                case Some(g) => updateGrid = g
              }
        }
      }
    }

    // ユニットの制約を伝搬させる
    for {unit <- unitMap.getOrElse(cellName, Seq.empty)} {
      val dPlaces = unit.filter(c => updateGrid.getOrElse(c, "").contains(digit))
      if (dPlaces.size == 0)
        return None // 矛盾。digitを置ける場所がなくなってしまった。
      else if (dPlaces.size == 1)
        // ユニットの中でdigitの置ける場所が一つしかないので、そこに割り当てる
        assign(updateGrid, dPlaces(0), digit) match {
          case None => return None // 矛盾が発生してしまった。
          case Some(g) => updateGrid = g
        }
    }

    Some(updateGrid)
  }

  /** 解を探索します。
    * @param 解く盤面
    */
  def search(gridOpt: Option[Map[String, String]]): Option[Map[String, String]] = 
    gridOpt match {
      case None => None
      case Some(grid) =>
        if (grid.forall(_._2.length == 1)) {
          gridOpt // 解けた
        } else {
          // 確定していないマスの中で、一番確定状態に近いマスを探す
          val (cellName, cellVal) = grid.filter(_._2.length > 1).minBy(_._2.length)
          // 可能性のあるパターンを、バックトラックしながら探索する。
          for (digit <- cellVal.split("")) {
            val searchedGridOpt = search(assign(grid, cellName, digit))
            if (!searchedGridOpt.isEmpty) return searchedGridOpt
          }
          None
        }
    }

  /** 数独を解きます。
    * @param gridStr 盤面の文字列。盤面の数字を行優先で並べた文字列。空白は「.」か「0」の文字にする事。改行や区切り文字もOK
    */
  def solve(gridStr: String): Option[Map[String, String]] = search(Some(parseGrid(gridStr)))
}

/** 数独を解くアプリケーション・クラスです。
  */
object SudokuApp {
  import Sudoku._

  /** main関数。最初の引数に、問題のデータを与えます。
    * 問題のデータは、行優先での数字の並びの文字列です。空白マスは、"0"または、"."です。それ以外の文字列は無視されます。
    */
  def main(args: Array[String]) =
    solve(args(0)) match {
      case None => println("Fail to solve.")
      case Some(grid) => display(grid)
    }

  /** 盤面を表示します。
    * @param grid 盤面
    */
  def display(grid: Map[String, String]): Unit = {
    val (cellName, digits) = grid.maxBy(taple => taple._2.length)
    val cellWidth = 1 + digits.length // マス目の幅は、数字列に空白を加えたもの
    val lineSeparator = ("-" * cellWidth * 3 + "+") * 3 // 3マスおきに+を加えて、9マス分の区切り行
    for (r <- rows.split("")) {
      println((for (c <- cols.split("")) yield {
        centerStr(grid.getOrElse(r + c, ""), cellWidth) + (if ("36".contains(c)) "|" else "")
      }).mkString)
      if ("CF".contains(r)) println(lineSeparator) // C、F行を表示したら区切り行を表示
    }
  }

  /** 文字列を指定された幅でセンタリングします。
    * @param str センタリングしたい文字列
    * @param width センタリングしたい領域の幅
    * 
    * @return 文字列の左右にセンタリングのための空白(" ")を追加します。
    */
  def centerStr(str: String, width: Int): String = {
    if (str.length == width) {
      str
    } else {
      val leftPad = " " * ((width - str.length) / 2) + str // 左の空白を付加
      leftPad + " " * (width - leftPad.length)             // 右の空白を付加
    }
  }
}
