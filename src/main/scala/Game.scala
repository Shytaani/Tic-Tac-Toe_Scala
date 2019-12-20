import java.util.Scanner
import scala.annotation.tailrec
import scala.Array._

object Game extends App {
  val board = Array.fill(3, 3)("　")
  val scanner = new Scanner(System.in)
  println(play(9))
  println()
  println("ゲームを終了します。")

  @tailrec
  def play(round: Int): String = {
    val player =
      round % 2 match {
        case 0 => "×"
        case _ => "○"
      }
    printBoard(oneRound(round, player))
    if (isGameOver(board, 0)) {
      s"${player}の勝利です。"
    } else if (round == 1) {
      "両者引き分けです。"
    } else {
      play(round - 1)
    }
  }

  private def oneRound(round: Int, player: String): Array[Array[String]] = {
    print(s"[${player}の番] 縦と横のインデックスを半角スペース区切りで指定してください。>")
    val inputs = scanner.nextLine().split(" ")
    if (inputs.length != 2) {
      println("エラー：整数は2つ入力してください。")
      println()
      oneRound(round, player)
    } else {
      try {
        val vertical = inputs(0).toInt
        val side = inputs(1).toInt
        if (vertical < 0 || vertical > 2 || side < 0 || side > 2) {
          println("エラー：整数は 0 ～ 2 の間で指定してください。")
          println()
          oneRound(round, player)
        } else if (board(vertical)(side) != "　") {
          println("エラー：指定された場所は既に入力されています。")
          println()
          oneRound(round, player)
        } else {
          board(vertical)(side) = player
          board
        }
      } catch {
        case e: NumberFormatException =>
          println("エラー：整数を入力してください。")
          println()
          oneRound(round, player)
      }
    }
  }

  private def printBoard(board: Array[Array[String]]): Unit = {
    println("┌─┬─┬─┐")
    println(s"│${board(0)(0)}│${board(0)(1)}│${board(0)(2)}│")
    println("├─┼─┼─┤")
    println(s"│${board(1)(0)}│${board(1)(1)}│${board(1)(2)}│")
    println("├─┼─┼─┤")
    println(s"│${board(2)(0)}│${board(2)(1)}│${board(2)(2)}│")
    println("└─┴─┴─┘")
    println()
    println()
  }

  @tailrec
  private def isGameOver(board: Array[Array[String]], index: Int): Boolean = {
    if (check(getLine(board, index)) || check(getRow(board, index))) true
    else if (index < 2) isGameOver(board, index + 1)
    else false
  }

  private def check(array: Array[String]): Boolean = array match {
    case Array("○", "○", "○") => true
    case Array("×", "×", "×") => true
    case _ => false
  }

  private def getLine(board: Array[Array[String]], index: Int): Array[String] = {
    Array(board(0)(index), board(1)(index), board(2)(index))
  }

  private def getRow(board: Array[Array[String]], index: Int): Array[String] = {
    Array(board(index)(0), board(index)(1), board(index)(2))
  }
}