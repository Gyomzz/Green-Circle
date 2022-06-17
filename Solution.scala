import math._
import scala.util._
import scala.io.StdIn._
import scala.collection.mutable._

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

case class Card(val name: String, var amount: Int)

class Application(val datas: Array[Int]) {
    val id: Int = datas(0)
    val ressources = new Array[Card](8)

    def setData() {
        for(i <- 1 until datas.length) {
            if(datas(i) >= 1) ressources(i - 1) = Card(CardType(i - 1).toString, datas(i)) else ressources(i - 1) = Card(CardType(i - 1).toString, 0)
        }
    }

    def getCards(): Array[Card] = {
        ressources.filter(_.amount != 0)
    }

    def getType(): String = {
        if(getCards.map(_.amount).sum <= 6) "small" else "big"
    }
    
    def getCardsNeeded(numberOfCompensation: Int, skillName: String): Array[Card] = {
        for(card <- ressources) {
            if(card.name == skillName) {
                card.amount = card.amount - (2 * numberOfCompensation)
            }
        }
        getCards
    }
}

class Player(details: Array[Int]) {
    val location: Int = details(0)
    val score: Int = details(1)
    val dailyRoutineCards: Int = details(2)
    val architectureStudyCards: Int = details(3)
    var acceptableDebt = 3

    def releaseApp(app: Application, cards: Array[Card], ennemyScore: Int): Unit = {
        if(shouldRelease(cards, app) || this.score == 4 || ennemyScore == 4) { 
            println("RELEASE " + app.id) 
        }
        else {
            Console.err.println("Debt too high : " +debtCount(cards, app) + " waiting")
            waiting
        }
    }

    def shouldRelease(cards: Array[Card], app: Application): Boolean = {
        debtCount(cards, app) < acceptableDebt
    }

    def debtCount(cards: Array[Card], app: Application): Int = {
        val numberOfBonus = Math.abs(cards.filter(_.name == "BONUS").length / 2)
        val cardsName = cards.map(_.name)
        val skillNeed = app.getCards
        var debt = 0
        for(skill <- skillNeed) {
            cards.find(_.name == skill.name) match {
                case Some(card) => if(card.amount != skill.amount / 2) debt += Math.abs(card.amount - (skill.amount / 2))
                case None => debt += 2
            }
        }
        debt -= numberOfBonus
        debt
    }
     
    def move(zoneId: Int): Unit = {
        Console.err.println("Moving to : " + CardType(zoneId))
        println("MOVE " + zoneId)
    }

    def giveCard(cardName: String): Unit = {
        Console.err.println("Giving card : " + cardName)
        println("GIVE " + getCardId(cardName))
    }

    def workApp(app: Application, ennemyLocation: Int, debtCard: Card): Unit = {
        val cardToGet = List(7, getCardId(Utils.getMissingSkills(app).head.name), 4, 6, 0).filter(_ != location)
        val cardToGetWithoutDebt = cardToGet.filter(_ != 7)
        val gonnaGiveCardToEnnemy = List(Math.abs(ennemyLocation - 1), ennemyLocation, Math.abs(ennemyLocation + 1))

        if(debtCard.amount > 4) move(getClosestSafeSpot(gonnaGiveCardToEnnemy, cardToGet))
        else move(getClosestSafeSpot(gonnaGiveCardToEnnemy, cardToGetWithoutDebt))
    }

    def getClosestSafeSpot(ennemySpot: List[Int], spots: List[Int]): Int = {
        var freeSpot: ListBuffer[Int] = ListBuffer()
        for(spot <- spots) {
            if(!ennemySpot.contains(spot)) {
                freeSpot += spot
            }
        }
        freeSpot.head
    }
    
    def getCardId(cardName: String): Int = {
        CardType.values.find(_.toString == cardName) match {
            case Some(card) => card.id
            case None => this.location
        }
    }   

    def random(): Unit = {
        println("RANDOM")
    }

    def waiting(): Unit = {
        println("WAIT")
    }
}

object Utils {
    val holdingCards = new Array[Card](10)
    
    def fillCard(cardsCount: Array[Int], cardsLocation: Array[Card]) {
        for(i <- 0 until cardsCount.length) cardsLocation(i) = Card(CardType(i).toString, cardsCount(i).toInt)
    }

    // skills 
    def findAppToRelease(apps: Array[Application]): Application = {
        getLowestSkillMissingApp(apps)
    }

    def getAmountOfSkillMissing(app: Application): Int = {
        val skillNeed = app.getCards
        var amountMissing = 0
        for(i <- 0 until skillNeed.length) {
            if(skillNeed(i).amount >= 1) {
                holdingCards.find(_.name == skillNeed(i).name) match {
                    case Some(card) => if(card.amount != skillNeed(i).amount / 2) amountMissing += (skillNeed(i).amount / 2) - card.amount
                    case None => amountMissing += 2
                }
            }
        }
        amountMissing
    }

    def appToWork(app1: Application, app2: Application): Application = {
        if(getMissingSkills(app1).length < getMissingSkills(app2).length && getMissingSkills(app1).length > 0) app1 else app2
    }

    def findAppToWork(apps: Array[Application]): Application = {
        apps.reduceLeft(appToWork)
    }

    def getMissingSkills(app: Application): Array[Card] = {
        val skillNeed = app.getCards
        val cardsNeeded = new Array[Card](skillNeed.length)
        for(i <- 0 until skillNeed.length) {
            holdingCards.find(_.name == skillNeed(i).name) match {
                case Some(card) => if(card.amount != skillNeed(i).amount / 2) cardsNeeded(i) = Card(skillNeed(i).name,skillNeed(i).amount / 2 - card.amount)
                case None => cardsNeeded(i) = Card(skillNeed(i).name, skillNeed(i).amount / 2)
            }
        }
        cardsNeeded.filter(_ != null)
    }

    def skillMissingApp(app1: Application, app2: Application): Application = {
        if(getAmountOfSkillMissing(app1) < getAmountOfSkillMissing(app2)) app1 else app2
    }

    def getLowestSkillMissingApp(apps: Array[Application]): Application = {
        apps.reduceLeft(skillMissingApp)
    }

    def skillCompensator(cards: Array[Card]): Int = {
        cards.find(_.name == "BONUS") match {
            case Some(card) => Math.floor(card.amount / 2).toInt
            case None => 0
        }
    }

    // cards 
    def noNeedCardForApp(app: Application): Array[String] = {
        holdingCards.filter(_.amount != 0).map(_.name).filter(!app.getCards.map(_.name).contains(_)).filter(_ != "TECHNICAL_DEBT").length match {
            case 0 => holdingCards.filter(_.amount != 0).map(_.name).filter(_ != "TECHNICAL_DEBT")
            case _ => holdingCards.filter(_.amount != 0).map(_.name).filter(!app.getCards.map(_.name).contains(_)).filter(_ != "TECHNICAL_DEBT")
        }
    }

    def findLeastNeededCard(app: Application): String = {
        noNeedCardForApp(app).head
    }

    def findPlayCards(): Array[String] = {
        val cards = holdingCards.filter(_.amount != 0).filter(_.name != "TECHNICAL_DEBT").map(_.name)
        val cardsToPlay = Array("TRAINING", "ARCHITECTURE_STUDY", "CODE_REVIEW", "REFACTORING")
        val playableCards = cards.filter(cardsToPlay.contains(_))
        playableCards
    }

    def playCard(): String = {
        Console.err.println("Playing Card : " + cardToPlayPriority.filter(findPlayCards.contains(_)).head)
        cardToPlayPriority.filter(findPlayCards.contains(_)).head
    }

    def cardToPlayPriority(): Array[String] = {
        if(holdingCards.filter(_.name == "TECHNICAL_DEBT").length != 0) Array("REFACTORING", "ARCHITECTURE_STUDY", "CODE_REVIEW", "TRAINING")
        else Array("ARCHITECTURE_STUDY", "CODE_REVIEW", "TRAINING", "REFACTORING")
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
        val dev = players(0)
        val boss = players(1)

        // ----- CARDS ----- //
        val myCardsInHand       = new Array[Card](10)
        val myDrawPile          = new Array[Card](10)
        val myDiscardPile       = new Array[Card](10)
        val myAutomatedCards    = new Array[Card](10)
        var debtCard = Card("TECHNICAL_DEBT", 0)
        val cardLocationsCount = readLine.toInt
        for(i <- 0 until cardLocationsCount) {
            // cardsLocation: the location of the card list. It can be HAND, DRAW, DISCARD or OPPONENT_CARDS (AUTOMATED and OPPONENT_AUTOMATED will appear in later leagues)
            val cardsDetails = readLine.split(" ")
            cardsDetails(0) match {
                case "HAND"         => 
                    Utils.fillCard(cardsDetails.tail.map (_.toInt), Utils.holdingCards)
                    Utils.fillCard(cardsDetails.tail.map (_.toInt), myCardsInHand)
                case "DRAW"         => Utils.fillCard(cardsDetails.tail.map (_.toInt), myDrawPile)
                case "DISCARD"      => Utils.fillCard(cardsDetails.tail.map (_.toInt), myDiscardPile)
                case "AUTOMATED"    => Utils.fillCard(cardsDetails.tail.map (_.toInt), myAutomatedCards)
                case _              => 
            }
            if(cardsDetails(0) == "HAND" || cardsDetails(0) == "DRAW" || cardsDetails(0) == "DISCARD" ) {
                debtCard.amount += cardsDetails(9).toInt
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
            case "MOVE"         => dev.workApp(Utils.findAppToWork(applications), boss.location, debtCard)
            case "RELEASE"      => dev.releaseApp(Utils.findAppToRelease(applications), Utils.holdingCards, boss.score)
            // Later League
            case "GIVE_CARD"    => dev.giveCard(Utils.findLeastNeededCard(Utils.findAppToRelease(applications)))
            case "THROW_CARD"   => 
            case "PLAY_CARD"    => println(Utils.playCard)
            case _              => println("RANDOM")
        }
    }
}

// In the first league: RANDOM | MOVE <zoneId> | RELEASE <applicationId> | WAIT; In later leagues: | GIVE <cardType> | THROW <cardType> | TRAINING | CODING | DAILY_ROUTINE | TASK_PRIORITIZATION <cardTypeToThrow> <cardTypeToTake> | ARCHITECTURE_STUDY | CONTINUOUS_DELIVERY <cardTypeToAutomate> | CODE_REVIEW | REFACTORING;