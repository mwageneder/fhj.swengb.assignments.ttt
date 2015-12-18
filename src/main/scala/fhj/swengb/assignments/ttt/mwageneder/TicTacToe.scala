package fhj.swengb.assignments.ttt.mwageneder

import scala.Predef
import scala.collection.Set

/**
  * models the vdifferent moves the game allows
  *
  * each move is made by either player a or player b.
  */

sealed trait TMove {
  def idx: Int
}

case object TopLeft extends TMove {
  override def idx: Int = 0
}

case object TopCenter extends TMove {
  override def idx: Int = 1
}

case object TopRight extends TMove {
  override def idx: Int = 2
}

case object MiddleLeft extends TMove {
  override def idx: Int = 3
}

case object MiddleCenter extends TMove {
  override def idx: Int = 4
}

case object MiddleRight extends TMove {
  override def idx: Int = 5
}

case object BottomLeft extends TMove {
  override def idx: Int = 6
}

case object BottomCenter extends TMove {
  override def idx: Int = 7
}

case object BottomRight extends TMove {
  override def idx: Int = 8
}


/**
  * for a tic tac toe game, there are two players, player A and player B
  */

sealed trait Player

case object PlayerA extends Player

case object PlayerB extends Player

object TicTacToe {

  /**
    * creates an empty tic tac toe game
    * @return
    */

  def apply(): TicTacToe = TicTacToe(Map())

  /**
    * For a given tic tac toe game, this function applies all moves to the game.
    * The first element of the sequence is also the first move.
    *
    * @param t
    * @param moves
    * @return
    */

  def play(t: TicTacToe, moves: Seq[TMove]): TicTacToe = {

    var player: Player = PlayerA

    for (move <- moves){
      t.turn(move, player)

      if(player.equals(PlayerA))
        player = PlayerB
      else
        player = PlayerA
    }
    return t
  }

  /**
    * creates all possible games.
    * @return
    */

  def mkGames(): Map[Seq[TMove], TicTacToe] = {

    ???

  }

}

/**
  * Models the well known tic tac toe game.
  *
  * The map holds the information which player controls which field.
  *
  * The nextplayer parameter defines which player makes the next move.
  */

case class TicTacToe (moveHistory: Map[TMove, Player],
                      nextPlayer: Player = PlayerA) {

  /**
   * outputs a representation of the tic tac toe like this:
   *
   * |---|---|---|
   * | x | o | x |
   * |---|---|---|
   * | o | x | x |
   * |---|---|---|
   * | x | o | o |
   * |---|---|---|
   *
   *
   * @return
   */

  def asString(): String = {


    val indexMap = Map(0 -> 16, 1 -> 20, 2 -> 24, 3 -> 44, 4 -> 48, 5 -> 52, 6 -> 72, 7 -> 76, 8 -> 80)


    var field: String = "|---|---|---|\n" +
      "|   |   |   |\n" +
      "|---|---|---|\n" +
      "|   |   |   |\n" +
      "|---|---|---|\n" +
      "|   |   |   |\n" +
      "|---|---|---|\n"

    val pos = Map(0 -> 16, 1 -> 20, 2 -> 24, 3 -> 44, 4 -> 48, 5 -> 52, 6 -> 72, 7 -> 76, 8 -> 80)


    for ((a, b) <- moveHistory) {
      if (b == PlayerA) {
        field = field.updated(indexMap(a.idx), "O").mkString
      }
      else if (b == PlayerB) {
        field = field.updated(indexMap(a.idx), "X").mkString
      }
      else {
        field = field.updated(indexMap(a.idx), "").mkString
      }
    }
    field
  }


  val winnings: List[Set[TMove]] = List(Set(TopLeft, TopCenter, TopRight),
    Set(MiddleLeft, MiddleCenter, MiddleRight),
    Set(BottomLeft, BottomCenter, BottomRight),
    Set(TopLeft, MiddleCenter, BottomRight),
    Set(TopRight, MiddleCenter, BottomLeft),
    Set(TopCenter, MiddleCenter, BottomCenter),
    Set(TopLeft, MiddleLeft, BottomLeft),
    Set(TopRight, MiddleRight, BottomRight))


  def checkIfWon(player: Player): Boolean = {

    if (winnings.head.subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else if (winnings(1).subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else if (winnings(2).subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else if (winnings(3).subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else if (winnings(4).subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else if (winnings(5).subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else if (winnings(6).subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else if (winnings(7).subsetOf(moveHistory.filter(_._2 == player).keySet))
      true
    else
      false
  }

  /**
   * the moves which are still to be played on this tic tac toe.
   */

  val playedMoves: Set[TMove] = Set(TopLeft, TopCenter, TopRight, MiddleLeft, MiddleCenter, MiddleRight, BottomLeft, BottomCenter, BottomRight)

  val unsetFields = moveHistory.filter(x => x._2 != PlayerA || x._2 != PlayerB).keySet

  val remainingMoves: Set[TMove] = playedMoves.diff(unsetFields)


  val gameOver: Boolean = true match {
    case gameEnd => winner.isDefined || remainingMoves.isEmpty
    case _ => false
  }

  /**
   * given a tic tac toe game, this function returns all
   * games which can be derived by making the next turn. that means one of the
   * possible turns is taken and added to the set.
   */

  lazy val nextGames: Set[TicTacToe] = {

    ???

  }

  /**
   * Either there is no winner, or PlayerA or PlayerB won the game.
   *
   * The set of moves contains all moves which contributed to the result.
   */

  def winner: Option[(Player, Set[TMove])] = {

    if (checkIfWon(PlayerA))
      Some(PlayerA, moveHistory.filter(_._2 == PlayerA).keySet)
    else if (checkIfWon(PlayerB))
      Some(PlayerB, moveHistory.filter(_._2 == PlayerB).keySet)
    else None
  }

  def turn(move: TMove, player: Player): TicTacToe = {

    if (!moveHistory.contains(move)) {
      if (player.equals(PlayerA))
        TicTacToe(moveHistory + (move -> player), PlayerB)
      else
        TicTacToe(moveHistory + (move -> player))
    }
    else
      TicTacToe(moveHistory)
  }


}