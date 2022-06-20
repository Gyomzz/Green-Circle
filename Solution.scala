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

class Team(val appsToRelease: Array[Application], val location: Int, val score: Int, val dailyCardsPlayed: Int, val archCardsPlayed: Int) {
    var deck: ListBuffer[Card] = new ListBuffer[Card]
    var hand: ListBuffer[Card] = new ListBuffer[Card]
    var draw: ListBuffer[Card] = new ListBuffer[Card]
    var acceptableDebt = 3

    def bonusAmount(): Int = {
        hand.filter(_.name == "BONUS").length
    }

    def debtAmount(): Int = {
        deck.filter(_.name == "TECHNICAL_DEBT").length
    }

    def appWithLeastDebt(): Application = {
        appsToRelease.reduceLeft(appDebt)
    }

    def appDebt(app1: Application, app2: Application): Application = {
        if(app1.debtCost(this) < app2.debtCost(this) ) app1 else app2
    }
    
    def shouldRelease(app: Application): Boolean = {
        if(almostFinish) acceptableDebt = 0
        app.debtCost(this) <= acceptableDebt
    }

    def almostFinish(): Boolean = {
        score == 4
    }

    def haveDebtInHand(): Boolean = {
        hand.filter(_.name == "TECHNICAL_DEBT").length > 0
    }

    def drawOnlyDebt(): Boolean = {
        draw.filter(_.name == "TECHNICAL_DEBT").length == draw.length
    }

    def shouldDraw(): Boolean = {
        !drawOnlyDebt && canDraw || drawOnlyDebt && hand.filter(_.name == "REFACTORING").length != 0
    }

    def canDraw(): Boolean = {
        draw.length > 2 
    }

    def haveCI(): Boolean = {
        hand.filter(_.name == "CONTINUOUS_INTEGRATION").length != 0
    }

    def haveMoreThanOneCI(): Boolean = {
        hand.filter(_.name == "CONTINUOUS_INTEGRATION").length > 1
    }

    def canCI(): Boolean = {
        hand.filter(_.name != "TECHNICAL_DEBT").length > 1
    }

    def canTaskPrio(): Boolean = {
        hand.filter(_.name == "BONUS").length != 0
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

    def addToDraw(card: Card, numberOfCards: Int, team: Team): Unit = {
        for(i <- 0 until numberOfCards) {
            team.draw += card
        }
    }

    def fillDeck(cards: Array[Int], team: Team): Unit ={
        for(i <- 0 until cards.length) {
            if(cards(i) != 0) addToDeck(Card(i, CardType.list(i).name), cards(i), team)
        }
    }

    def fillDraw(cards: Array[Int], team: Team): Unit ={
        for(i <- 0 until cards.length) {
            if(cards(i) != 0) addToDraw(Card(i, CardType.list(i).name), cards(i), team)
        }
    }
    
    def fillHand(cards: Array[Int], team: Team): Unit ={
        for(i <- 0 until cards.length) {
            if(cards(i) != 0) addToHand(Card(i, CardType.list(i).name), cards(i), team)
        }
    }
}

case class Need(val typeId: Int, val name: String, var amount: Int)

object Needs {
    val list: List[Need] = List(
        Need(0, "TRAINING", 0),
        Need(1, "CODING", 0),
        Need(2, "DAILY_ROUTINE", 0),
        Need(3, "TASK_PRIORITIZATION", 0),
        Need(4, "ARCHITECTURE_STUDY", 0),
        Need(5, "CONTINUOUS_INTEGRATION", 0),
        Need(6, "CODE_REVIEW", 0),
        Need(7, "REFACTORING", 0),
        Need(8, "BONUS", 0)
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
        list.filter(_.amount != 0).sortWith(_.amount > _.amount)
    }

    def leastWanted(): List[Need] = {
        list.sortWith(_.amount < _.amount)
    }
}

object Choices {
    val cardPriority = List("REFACTORING", "CONTINUOUS_INTEGRATION", "CODING", "ARCHITECTURE_STUDY",  "DAILY_ROUTINE", "TASK_PRIORITIZATION",  "TRAINING",  "CODE_REVIEW")

    def cardsValue(team: Team, ennemyTeam: Team): List[String] = {
        var cardToPlay = cardPriority
        if(team.score >= ennemyTeam.score + 1 || team.debtAmount == 0) 
        cardToPlay = cardToPlay.filter(_ != "REFACTORING")
        if(!team.shouldDraw) cardToPlay = cardToPlay.filter(_ != "CODING")
        if(!team.haveCI || !team.canCI) cardToPlay = cardToPlay.filter(_ != "CONTINUOUS_INTEGRATION")
        if(!team.canTaskPrio) cardToPlay = cardToPlay.filter(_ != "TASK_PRIORITIZATION")
        if(!team.shouldDraw) cardToPlay = cardToPlay.filter(_ != "TRAINING")
        cardToPlay
    }

    def playBestCard(team: Team, ennemyTeam: Team): Unit = {
        if(team.shouldRelease(team.appWithLeastDebt)) println("WAIT")
        else if(team.hand.map(_.name).filter(cardsValue(team, ennemyTeam).contains(_)).length != 0) {
            team.hand.map(_.name).filter(cardsValue(team, ennemyTeam).contains(_)).head match {
                case "CONTINUOUS_INTEGRATION" => {
                    if(team.bonusAmount != 0) println("CONTINUOUS_INTEGRATION " + 8)
                    else if(Needs.mostWanted.filter(team.hand.contains(_)).length != 0)
                        println("CONTINUOUS_INTEGRATION " + Needs.mostWanted.filter(team.hand.contains(_)).head.typeId)
                    else {
                        if(team.haveMoreThanOneCI) 
                        println("CONTINUOUS_INTEGRATION " + team.hand.filter(_.name != "TECHNICAL_DEBT").head.id)
                        else
                        println("CONTINUOUS_INTEGRATION " + 
                        team.hand.filter(_.name != "TECHNICAL_DEBT").filter(_.name != "CONTINUOUS_INTEGRATION").head.id)
                    } 
                }
                case "TASK_PRIORITIZATION" => println("TASK_PRIORITIZATION " + 8 + " " + Needs.mostWanted.head.typeId)
                case "DAILY_ROUTINE" => println("DAILY_ROUTINE")
                case _ => println(team.hand.map(_.name).filter(cardsValue(team, ennemyTeam).contains(_)).head)
            }
        }
        else println("WAIT")
    }

    def leastNeedCard(team: Team): Card = {
        getCardByName(team.hand.map(_.name)
            .filter(Needs.leastWanted.map(_.name).contains(_))
            .head
        )
    }

    def getCardByName(cardName: String): Card = {
        CardType.list.filter(_.name == cardName).head
    }

    def findBestZone(team: Team, disturbLocation: List[Int]): Int = {
        if(team.debtAmount > 4) closestSpot(team.location, List(1, 2, 3, 4, 5, 7).filter(!disturbLocation.contains(_)))
        else closestSpot(team.location, List(1, 2, 3, 4, 5).filter(!disturbLocation.contains(_)))
    }

    def closestSpot(teamLocation: Int, mostWantedZone: List[Int]): Int = {
        if(teamLocation != 7 && mostWantedZone.filter(_ > teamLocation).length != 0) 
            mostWantedZone.filter(_ > teamLocation).min
        else 
            mostWantedZone.filter(_ < teamLocation).min
    }

    def daily(team: Team, disturbLocation: List[Int]): Unit = {
        Console.err.println("Daily is ok")
        if(Choices.findBestZone(team, disturbLocation) + team.dailyCardsPlayed == 4 && 
        Choices.findBestZone(team, disturbLocation) != 7) 
            println("MOVE " + Choices.findBestZone(team, disturbLocation) + " 4")
        else if(
            Choices.findBestZone(team, disturbLocation) + team.dailyCardsPlayed == 5 && 
            Choices.findBestZone(team, disturbLocation) != 7 || 
            Choices.findBestZone(team, disturbLocation) - team.dailyCardsPlayed == 5 && 
            Choices.findBestZone(team, disturbLocation) != 7
        ) println("MOVE " + Choices.findBestZone(team, disturbLocation) + " 5")
        else println("MOVE " + Choices.findBestZone(team, disturbLocation))
    }
}

object GamePhase {
    def move(team: Team, disturbLocation: List[Int]): Unit = {
        if(team.dailyCardsPlayed != 0) {
            Choices.daily(team, disturbLocation)
        } else {
            println("MOVE " + Choices.findBestZone(team, disturbLocation))
        }
    }

    def release(team: Team): Unit = {
        if(team.shouldRelease(team.appWithLeastDebt)) println("RELEASE " + team.appWithLeastDebt.id)
        else println("WAIT")
    }

    def giveCard(team: Team): Unit = {
        Console.err.println("giving : " + Choices.leastNeedCard(team).name)
        println("GIVE " + Choices.leastNeedCard(team).id)
    }

    def throwCard(team: Team): Unit = {
        Console.err.println("trhowing : " + Choices.leastNeedCard(team).name)
        println("THROW " + Choices.leastNeedCard(team).id)
    }

    def playCard(team: Team, ennemyTeam: Team): Unit = {
        Choices.playBestCard(team, ennemyTeam)
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
                case "DRAW" => {
                    Filler.fillDeck( cardDetails.tail.map(_.toInt), myTeam)
                    Filler.fillDraw( cardDetails.tail.map(_.toInt), myTeam)
                }
                case "DISCARD" => Filler.fillDeck( cardDetails.tail.map(_.toInt), myTeam)
                case "OPPONENT_CARDS" =>  Filler.fillDeck( cardDetails.tail.map(_.toInt), ennemyTeam)
                case _ => 
            }
        }

        // --- NEEDS --- //
        Needs.resetNeed
        applications.foreach(Needs.update)

        // --- MOVES --- //
        val possibleMovesCount = readLine.toInt
        for(i <- 0 until possibleMovesCount) {
            val possibleMove = readLine
        }
        val disturbLocation = List(Math.abs(ennemyTeam.location - 1), ennemyTeam.location, Math.abs(ennemyTeam.location + 1))
        Console.err.println(myTeam.dailyCardsPlayed)
        Console.err.println(myTeam.archCardsPlayed)
        gamePhase match {
            case "MOVE"         => GamePhase.move(myTeam, disturbLocation)
            case "RELEASE"      => GamePhase.release(myTeam)
            case "GIVE_CARD"    => GamePhase.giveCard(myTeam)
            case "THROW_CARD"   => GamePhase.throwCard(myTeam)
            case "PLAY_CARD"    => GamePhase.playCard(myTeam, ennemyTeam)
            case _              => 
        }
        

        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        

        // In the first league: RANDOM | MOVE <zoneId> | RELEASE <applicationId> | WAIT; In later leagues: | GIVE <cardType> | THROW <cardType> | TRAINING | CODING | DAILY_ROUTINE | TASK_PRIORITIZATION <cardTypeToThrow> <cardTypeToTake> | ARCHITECTURE_STUDY | CONTINUOUS_DELIVERY <cardTypeToAutomate> | CODE_REVIEW | REFACTORING;
    }
}
