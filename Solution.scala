import math._
import scala.util._
import scala.io.StdIn._

object CardType extends Enumeration {
    type CardType = Value
    val TRAINING, CODING, DAILY_ROUTINE, TASK_PRIORITIZATION, ARCHITECTURE_STUDY, CONTINUOUS_INTEGRATION, CODE_REVIEW, REFACTORING, BONUS, TECHNICAL_DEBT = Value
}

class Application(val datas: Array[Int]) {
    val id: Int = datas(1)
    val ressources = new Array[Int](8)

    def setData() {
        for(i <- 1 until datas.length) {
            ressources(i - 1) = datas(i)
        }
    }
}

class Player(details: Array[Int]) {
    val location: Int = details(0)
    val score: Int = details(1)
    val dailyRoutineCards: Int = details(2)
    val architectureStudyCards: Int = details(3)
}

object Utils {

    def fillCard(cardsCount: Array[Int], cardsLocation: Array[Int]) {
        for(i <- 0 until cardsCount.length) cardsLocation(i) = cardsCount(i).toInt
    }
}

/**
 * Complete the hackathon before your opponent by following the principles of Green IT
 **/
object Player extends App {

    // game loop
    while(true) {
        val gamePhase = readLine // can be MOVE, GIVE_CARD, THROW_CARD, PLAY_CARD or RELEASE

        // ----- APPLICATIONS ----- //
        val applicationsCount = readLine.toInt
        val applications = new Array[Application](applicationsCount)
        for(i <- 0 until applicationsCount) {
            applications(i) = new Application(readLine.split(" ").filter(_ != "APPLICATION").map (_.toInt))
            applications(i).setData
        }

        // ----- PLAYER ----- //
        val players = new Array[Player](2)
        for(i <- 0 until 2) {
            players(i) = new Player((readLine split " ").filter(_ != "").map (_.toInt))
        }

        // ----- CARDS ----- //
        val myCardsInHand       = new Array[Int](10)
        val myDrawPile          = new Array[Int](10)
        val myDiscardPile       = new Array[Int](10)
        val myAutomatedCards    = new Array[Int](10)

        val cardLocationsCount = readLine.toInt
        for(i <- 0 until cardLocationsCount) {
            val cardsDetails = readLine.split(" ")
            cardsDetails(0) match {
                case "HAND"         => Utils.fillCard(cardsDetails.tail.map (_.toInt), myCardsInHand)
                case "DRAW"         => Utils.fillCard(cardsDetails.tail.map (_.toInt), myDrawPile)
                case "DISCARD"      => Utils.fillCard(cardsDetails.tail.map (_.toInt), myDiscardPile)
                case "AUTOMATED"    => Utils.fillCard(cardsDetails.tail.map (_.toInt), myAutomatedCards)
                case _              => 
            }
        }
        
        // ----- MOVES ----- //
        val movesCount = readLine.toInt
        val moves = new Array[String](movesCount)
        for(i <- 0 until movesCount) {
            moves(i) = readLine
        }

        gamePhase match {
            // First League
            case "MOVE"         => println("RANDOM")
            case "RELEASE"      => println("RANDOM")
            // Later League
            case "GIVE_CARD"    => println("RANDOM")
            case "THROW_CARD"   => println("RANDOM")
            case "PLAY_CARD"    => println("RANDOM")
            case _              => println("RANDOM")
        }
    }
}