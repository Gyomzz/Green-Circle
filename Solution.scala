import math._
import scala.util._
import scala.io.StdIn._
import scala.collection.mutable._

case class Skill(val id: Int, val name: String, val amountNeeded: Int)

class Application(val id: Int, val skillsNeeds: Array[Skill]) {

    def cost(): Int = {
        skillsNeeds.map(_.amountNeeded).sum
    }

    def numberOfCardsForSkill(skillName: String): Int = {
        skillsNeeds.filter(_.name == skillName).map(_.amountNeeded).sum / 2
    }

    def skillsRequirement(): Array[String] = {
        skillsNeeds.filter(_.amountNeeded != 0).map(_.name )
    }

    def technicalPoints(teamHand: ListBuffer[Card], teamBonusAmount: Int): Int = {
        var tech = 0
        for(skill <- skillsNeeds) {
            tech += (teamHand.filter(_.name == skill.name).take(skill.amountNeeded / 2).length * 2)  
        }
        tech += teamBonusAmount
        tech
    }

    def debtCost(team: Team): Int = {
        if(cost < technicalPoints(team.hand, team.bonusAmount)) 0 else cost - technicalPoints(team.hand, team.bonusAmount)
    }
}

case class Card(val id: Int, val name: String)

class Team(val appsToRelease: Array[Application], val location: Int, val score: Int, val dailyCardPlayed: Int, val archCardsPlayed: Int) {
    var deck: ListBuffer[Card] = new ListBuffer[Card]
    var hand: ListBuffer[Card] = new ListBuffer[Card]

    def addToDeck(card: Card): Unit = {
        deck += card
    }

    def addToHand(card: Card): Unit = {
        hand += card
    }

    def bonusAmount(): Int = {
        hand.filter(_.name == "BONUS").length
    }

    def appWithLeastDebt(): Application = {
        appsToRelease.reduceLeft(appDebt)
    }

    def appDebt(app1: Application, app2: Application): Application = {
        if(app1.debtCost(this) < app2.debtCost(this) ) app1 else app2
    }
}

object CardType extends Enumeration {
    type CardType = Value
    val TRAINING                = Value(0, "TRAINING")
    val CODING                  = Value(1, "CODING")
    val DAILY_ROUTINE           = Value(2, "DAILY_ROUTINE")
    val TASK_PRIORITIZATION     = Value(3, "TASK_PRIORITIZATION")
    val ARCHITECTURE_STUDY      = Value(4, "ARCHITECTURE_STUDY")
    val CONTINUOUS_INTEGRATION  = Value(5, "CONTINUOUS_INTEGRATION")
    val CODE_REVIEW             = Value(6, "CODE_REVIEW")
    val REFACTORING             = Value(7, "REFACTORING")
    val BONUS                   = Value(8, "BONUS")
    val TECHNICAL_DEBT          = Value(9, "TECHNICAL_DEBT")
}

object Filler {
    def addToDeck(card: Card, numberOfCards: Int, team: Team): Unit = {
        for(i <- 0 until numberOfCards) {
            team.addToDeck(card)
        }
    }

    def addToHand(card: Card, numberOfCards: Int, team: Team): Unit = {
        for(i <- 0 until numberOfCards) {
            team.addToHand(card)
        }
    }

    def fillDeck(cards: Array[Int], team: Team): Unit ={
        for(i <- 0 until cards.length) {
            if(cards(i) != 0) addToDeck(Card(i, CardType(i).toString), cards(i), team)
        }
    }
    
    def fillHand(cards: Array[Int], team: Team): Unit ={
        for(i <- 0 until cards.length) {
            if(cards(i) != 0) addToHand(Card(i, CardType(i).toString), cards(i), team)
        }
    }
}

object GamePhase {
    def move(): Unit = {
        println("RANDOM")
    }

    def release(team: Team): Unit = {
        println("RELEASE " + team.appWithLeastDebt.id)
    }

    def giveCard(): Unit = {
        println("RANDOM")
    }

    def throwCard(): Unit = {
        println("RANDOM")
    }

    def playCard(): Unit = {
        println("WAIT")
    }
}

/**
 * Complete the hackathon before your opponent by following the principles of Green IT
 **/
object Player extends App {

    // game loop
    while(true) {
        val gamePhase = readLine // can be MOVE, GIVE_CARD, THROW_CARD, PLAY_CARD or RELEASE

        // --- APPLICATIONS --- //
        val applicationsCount = readLine.toInt
        val applications = new Array[Application](applicationsCount)
        for(i <- 0 until applicationsCount) {
            // trainingNeeded: number of TRAINING skills needed to release this application
            // codingNeeded: number of CODING skills needed to release this application
            // dailyRoutineNeeded: number of DAILY_ROUTINE skills needed to release this application
            // taskPrioritizationNeeded: number of TASK_PRIORITIZATION skills needed to release this application
            // architectureStudyNeeded: number of ARCHITECTURE_STUDY skills needed to release this application
            // continuousDeliveryNeeded: number of CONTINUOUS_DELIVERY skills needed to release this application
            // codeReviewNeeded: number of CODE_REVIEW skills needed to release this application
            // refactoringNeeded: number of REFACTORING skills needed to release this application
            val Array(objectType, _id, _trainingNeeded, _codingNeeded, _dailyRoutineNeeded, _taskPrioritizationNeeded, _architectureStudyNeeded, _continuousDeliveryNeeded, _codeReviewNeeded, _refactoringNeeded) = readLine split " "
            val id = _id.toInt
            val trainingNeeded = _trainingNeeded.toInt
            val codingNeeded = _codingNeeded.toInt
            val dailyRoutineNeeded = _dailyRoutineNeeded.toInt
            val taskPrioritizationNeeded = _taskPrioritizationNeeded.toInt
            val architectureStudyNeeded = _architectureStudyNeeded.toInt
            val continuousDeliveryNeeded = _continuousDeliveryNeeded.toInt
            val codeReviewNeeded = _codeReviewNeeded.toInt
            val refactoringNeeded = _refactoringNeeded.toInt

            applications(i) = new Application(id, Array(
                Skill(0, "TRAINING", trainingNeeded.toInt),
                Skill(1, "CODING", codingNeeded.toInt),
                Skill(2, "DAILY_ROUTINE", dailyRoutineNeeded.toInt),
                Skill(3, "TASK_PRIORITIZATION", taskPrioritizationNeeded.toInt),
                Skill(4, "ARCHITECTURE_STUDY", architectureStudyNeeded.toInt),
                Skill(5, "CONTINUOUS_INTEGRATION", continuousDeliveryNeeded.toInt),
                Skill(6, "CODE_REVIEW", codeReviewNeeded.toInt),
                Skill(7, "REFACTORING", refactoringNeeded.toInt)
                )
            )
        }

        // --- PLAYER --- //
        val companies = new Array[Team](2)
        for(i <- 0 until 2) {
            // playerLocation: id of the zone in which the player is located
            // playerPermanentDailyRoutineCards: number of DAILY_ROUTINE the player has played. It allows them to take cards from the adjacent zones
            // playerPermanentArchitectureStudyCards: number of ARCHITECTURE_STUDY the player has played. It allows them to draw more cards
            val Array(playerLocation, playerScore, playerPermanentDailyRoutineCards, playerPermanentArchitectureStudyCards) = (readLine split " ").filter(_ != "").map (_.toInt)
            companies(i) = new Team(applications.sortWith(_.cost < _.cost), playerLocation, playerScore, playerPermanentDailyRoutineCards, playerPermanentArchitectureStudyCards)
        }
        val myTeam = companies(0)
        val ennemyTeam = companies(1)

        // --- CARDS --- //
        val cardLocationsCount = readLine.toInt
        for(i <- 0 until cardLocationsCount) {
            // cardsLocation: the location of the card list. It can be HAND, DRAW, DISCARD or OPPONENT_CARDS (AUTOMATED and OPPONENT_AUTOMATED will appear in later leagues)
            val cardDetails: Array[String] = readLine split " "
            
            cardDetails(0) match {
                case "HAND" => {
                    Filler.fillDeck( cardDetails.tail.map(_.toInt), myTeam)
                    Filler.fillHand( cardDetails.tail.map(_.toInt), myTeam)
                }
                case "DRAW" => Filler.fillDeck( cardDetails.tail.map(_.toInt), myTeam)
                case "DISCARD" => Filler.fillDeck( cardDetails.tail.map(_.toInt), myTeam)
                case "OPPONENT_CARDS" =>  Filler.fillDeck( cardDetails.tail.map(_.toInt), ennemyTeam)
                case _ => 
            }
        }

        // --- MOVES --- //
        val possibleMovesCount = readLine.toInt
        for(i <- 0 until possibleMovesCount) {
            val possibleMove = readLine
        }

        gamePhase match {
            case "MOVE"         => GamePhase.move
            case "RELEASE"      => GamePhase.release(myTeam)
            case "GIVE_CARD"    => GamePhase.giveCard
            case "THROW_CARD"   => GamePhase.throwCard
            case "PLAY_CARD"    => GamePhase.playCard
            case _              => 
        }
        

        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        

        // In the first league: RANDOM | MOVE <zoneId> | RELEASE <applicationId> | WAIT; In later leagues: | GIVE <cardType> | THROW <cardType> | TRAINING | CODING | DAILY_ROUTINE | TASK_PRIORITIZATION <cardTypeToThrow> <cardTypeToTake> | ARCHITECTURE_STUDY | CONTINUOUS_DELIVERY <cardTypeToAutomate> | CODE_REVIEW | REFACTORING;
    }
}