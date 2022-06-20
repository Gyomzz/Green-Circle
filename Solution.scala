import math._
import scala.util._
import scala.io.StdIn._
import scala.collection.mutable._

case class Skill(val id: Int, val name: String, val amountNeeded: Int)

class Application(val id: Int, val skillsNeeds: Array[Skill]) {

    def cost(): Int = {
        skillsNeeds.map(_.amountNeeded).sum
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

    def bonusAmount(): Int = {
        hand.filter(_.name == "BONUS").length
    }

    def appWithLeastDebt(): Application = {
        appsToRelease.reduceLeft(appDebt)
    }

    def appDebt(app1: Application, app2: Application): Application = {
        if(app1.debtCost(this) < app2.debtCost(this) ) app1 else app2
    }
    
    def shouldRelease(app: Application): Boolean = {
        app.debtCost(this) <= 3
    }
}

object CardType {
    val list = List(
        Card(0, "TRAINING"),
        Card(1, "CODING"),
        Card(2, "DAILY_ROUTINE"),
        Card(3, "TASK_PRIORITIZATION"),
        Card(4, "ARCHITECTURE_STUDY"),
        Card(5, "CONTINUOUS_INTEGRATION"),
        Card(6, "CODE_REVIEW"),
        Card(7, "REFACTORING"),
        Card(8, "BONUS"),
        Card(9, "TECHNICAL_DEBT")
    )
}

object Filler {
    def addToDeck(card: Card, numberOfCards: Int, team: Team): Unit = {
        for(i <- 0 until numberOfCards) {
            team.deck += card
        }
    }

    def addToHand(card: Card, numberOfCards: Int, team: Team): Unit = {
        for(i <- 0 until numberOfCards) {
            team.hand += card
        }
    }

    def fillDeck(cards: Array[Int], team: Team): Unit ={
        for(i <- 0 until cards.length) {
            if(cards(i) != 0) addToDeck(Card(i, CardType.list(i).name), cards(i), team)
        }
    }
    
    def fillHand(cards: Array[Int], team: Team): Unit ={
        for(i <- 0 until cards.length) {
            if(cards(i) != 0) addToHand(Card(i, CardType.list(i).name), cards(i), team)
        }
    }
}

case class Need(val name: String, var amount: Int)

object Needs {
    val list: List[Need] = List(
        Need("TRAINING", 0),
        Need("CODING", 0),
        Need("DAILY_ROUTINE", 0),
        Need("TASK_PRIORITIZATION", 0),
        Need("ARCHITECTURE_STUDY", 0),
        Need("CONTINUOUS_INTEGRATION", 0),
        Need("CODE_REVIEW", 0),
        Need("REFACTORING", 0)
    )

    def update(app: Application): Unit = {
        for(skill <- app.skillsNeeds) {
            list.find(_.name == skill.name) match {
                case Some(need) => need.amount += skill.amountNeeded
                case _ =>
            }
        }
    }

    def resetNeed(): Unit = {
        list.foreach(_.amount = 0)
    }

    def mostWanted(): List[Need] = {
        list.sortWith(_.amount > _.amount)
    }

    def leastWanted(): List[Need] = {
        list.sortWith(_.amount < _.amount)
    }
}

object Choices {
    def leastNeedCard(team: Team, leastNeededCards: List[Need]): Card = {
        if(team.bonusAmount != 0) getCardByName("BONUS") else
        getCardByName(team.hand.map(_.name)
            .filter(leastNeededCards.map(_.name).contains(_))
            .head
        )
    }

    def getCardByName(cardName: String): Card = {
        CardType.list.filter(_.name == cardName).head
    }
}

object GamePhase {
    def move(move: String): Unit = {
        println(move)
    }

    def release(team: Team): Unit = {
        if(team.shouldRelease(team.appWithLeastDebt)) println("RELEASE " + team.appWithLeastDebt.id)
        else println("WAIT")
    }

    def giveCard(team: Team): Unit = {
        Console.err.println("giving : " + Choices.leastNeedCard(team, Needs.leastWanted).name)
        println("GIVE " + Choices.leastNeedCard(team, Needs.leastWanted).id)
    }

    def throwCard(team: Team): Unit = {
        Console.err.println("trhowing : " + Choices.leastNeedCard(team, Needs.leastWanted).name)
        println("THROW " + Choices.leastNeedCard(team, Needs.leastWanted).id)
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

        Needs.resetNeed
        applications.foreach(Needs.update)

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
        val moves = new Array[String](possibleMovesCount)
        for(i <- 0 until possibleMovesCount) {
            val possibleMove = readLine
            moves(i) = possibleMove
        }

        gamePhase match {
            case "MOVE"         => GamePhase.move(moves.head)
            case "RELEASE"      => GamePhase.release(myTeam)
            case "GIVE_CARD"    => GamePhase.giveCard(myTeam)
            case "THROW_CARD"   => GamePhase.throwCard(myTeam)
            case "PLAY_CARD"    => GamePhase.playCard
            case _              => 
        }
        

        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        

        // In the first league: RANDOM | MOVE <zoneId> | RELEASE <applicationId> | WAIT; In later leagues: | GIVE <cardType> | THROW <cardType> | TRAINING | CODING | DAILY_ROUTINE | TASK_PRIORITIZATION <cardTypeToThrow> <cardTypeToTake> | ARCHITECTURE_STUDY | CONTINUOUS_DELIVERY <cardTypeToAutomate> | CODE_REVIEW | REFACTORING;
    }
}
