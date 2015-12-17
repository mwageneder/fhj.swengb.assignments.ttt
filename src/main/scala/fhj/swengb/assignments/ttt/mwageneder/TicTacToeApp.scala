package fhj.swengb.assignments.ttt.mwageneder

import java.net.URL
import java.util.ResourceBundle
import javafx.application.Application
import javafx.event.EventHandler
import javafx.fxml.{FXML, Initializable, FXMLLoader}
import javafx.scene.control.{Button, Label}
import javafx.scene.input.MouseEvent
import javafx.scene.layout.{GridPane}
import javafx.scene.{Scene, Parent}
import javafx.stage.Stage

import scala.util.control.NonFatal

object TicTacToeApp {
  def main(args: Array[String]) {
    Application.launch(classOf[TicTacToeApp], args: _*)
  }
}

class TicTacToeApp extends javafx.application.Application {

  val Fxml = "/fhj/swengb/assignments/ttt/TicTacToeApp.fxml"

  val loader = new FXMLLoader(getClass.getResource(Fxml))

  override def start(stage: Stage): Unit = try {
    stage.setTitle("Maxl's TicTacToe Game")
    loader.load[Parent]() // side effect
    val scene = new Scene(loader.getRoot[Parent])
    stage.setScene(scene)
    stage.show()

  } catch {
    case NonFatal(e) => {
      e.printStackTrace()
    }
  }
}

class TicTacToeAppController extends Initializable {

  @FXML var gridpane : GridPane = _

  @FXML var topleft: Label = _
  @FXML var topcenter: Label = _
  @FXML var topright: Label = _
  @FXML var middleleft: Label = _
  @FXML var middlecenter: Label = _
  @FXML var middleright: Label = _
  @FXML var bottomleft: Label = _
  @FXML var bottomcenter: Label = _
  @FXML var bottomright: Label = _

  @FXML var msglabel: Label = _
  @FXML var btnnewgame: Button = _


  override def initialize(location: URL, resources: ResourceBundle): Unit = {

    topleft.setOnMouseClicked(mouseEventHandler) ; topleft.setUserData(0)
    topcenter.setOnMouseClicked(mouseEventHandler); topcenter.setUserData(1)
    topright.setOnMouseClicked(mouseEventHandler); topright.setUserData(2)
    middleleft.setOnMouseClicked(mouseEventHandler); middleleft.setUserData(3)
    middlecenter.setOnMouseClicked(mouseEventHandler); middlecenter.setUserData(4)
    middleright.setOnMouseClicked(mouseEventHandler) ; middleright.setUserData(5)
    bottomleft.setOnMouseClicked(mouseEventHandler) ; bottomleft.setUserData(6)
    bottomcenter.setOnMouseClicked(mouseEventHandler) ; bottomcenter.setUserData(7)
    bottomright.setOnMouseClicked(mouseEventHandler) ;  bottomright.setUserData(8)

    gridpane.setDisable(true)
    btnnewgame.setOnMouseClicked(mouseEventHandler)
    msglabel.setText("To start a New Game click the Button")
  }

  var newGame = TicTacToe.apply()

  val labelmoveMap: Map[Int, (Label, TMove)] = Map( 0 -> (topleft,TopLeft),
                                                    1 -> (topcenter,TopCenter),
                                                    2 -> (topright,TopRight),
                                                    3 -> (middleleft,MiddleLeft),
                                                    4 -> (middlecenter,MiddleCenter),
                                                    5 -> (middleright,MiddleRight),
                                                    6 -> (bottomleft,BottomLeft),
                                                    7 -> (bottomcenter,BottomCenter),
                                                    8 -> (bottomright,BottomRight))

  val mouseEventHandler: EventHandler[_ >: MouseEvent] = new EventHandler[MouseEvent] {

    override def handle(event: MouseEvent): Unit = {
      event.getSource match {
        case onclick: Label => {

          if (newGame.gameOver)
            gameOver()

          if (newGame.nextPlayer == PlayerA) {
            val a = labelmoveMap.get(onclick.getUserData.toString.toInt)

            if(newGame.remainingMoves.contains(a.get._2)){
              onclick.setText("x")
              newGame = newGame.turn(a.get._2,newGame.nextPlayer)
              gameOver()
            }
          }

          else if (newGame.nextPlayer == PlayerB) {

            if(newGame.remainingMoves.contains(labelmoveMap(onclick.getUserData.toString.toInt)._2)){
              onclick.setText("o")
              newGame = newGame.turn(labelmoveMap(onclick.getUserData.toString.toInt)._2,newGame.nextPlayer)
              gameOver()

            }
          }
        }

        case btnGame: Button if btnnewgame == btnnewgame => {

          newGame = TicTacToe.apply()
          gridpane.setDisable(false)

          bottomcenter.setText(null)
          bottomleft.setText(null)
          bottomright.setText(null)
          middleleft.setText(null)
          middlecenter.setText(null)
          middleright.setText(null)
          topleft.setText(null)
          topcenter.setText(null)
          topright.setText(null)
          msglabel.setText("[New Game started]")
        }

        case _ => assert(false)
      }
    }
  }

  def gameOver(): Unit = {

    if (newGame.gameOver){

      if(!newGame.checkIfWon(PlayerA) && !newGame.checkIfWon(PlayerB)) {
        msglabel.setText("Draw! --> to play again press the Button")
        gridpane.setDisable(true)
      }
      else if(newGame.winner.get._1.equals(PlayerA)) {
        msglabel.setText("X - Player 1 won!")
        gridpane.setDisable(true)
      }
      else if(newGame.winner.get._1.equals(PlayerB)) {
        msglabel.setText("O - Player 2 won!")
        gridpane.setDisable(true)
      }
    }
  }
}