import math._
import scala.util._
import scala.io.StdIn._
import scala.collection.mutable._

case class Skill(val id: Int, val name: String, val amountNeeded: Int)

class Application(val id: Int, val skillsNeeds: Array[Skill]) {
    def cost(): Int = {
        skillsNeeds.map(_.amountNeeded).sum
    }

    // def canRelease(team: Team): Boolean = {
    //     cost <= team.handValue
    // }

    def debtIfRelease(teamHand: ArrayBuffer[Card]): Int = {
        for(skill <- skillsNeeds) {
            if(enoughGoodCard(skill.amountNeeded, teamHand.filter(_.name == skill.name).length)) 
            teamHand.filter(_.name == skill.name).foreach(_.points += 2)
        }
        if(canReleaseWithoutDebt(teamHand, teamHand.filter(_.name == "BONUS").length)) 0 else cost - teamHand.map(_.points).sum    
    }

    def canReleaseWithoutDebt(teamHand: ArrayBuffer[Card], teamBonusAmount: Int): Boolean = {
        cost > teamHand.filter(_.name != "BONUS").filter(_.name != "TECHNICAL_DEBT").map(_.points).sum + teamBonusAmount
    }

    def countCardPoint(skillPoint: Int, cardAmount: Int): Int = {
        cardAmount - (skillPoint / 2)
    }

    def enoughGoodCard(skillPoint: Int, cardAmount: Int): Boolean = {
        cardAmount >= skillPoint / 2
    }
}

case class Card(val id: Int, val name: String) {
    var points = 0
}

class Team(val appToRelease: Array[Application], val location: Int, val score: Int, val dailyCardPlayed: Int, val archCardsPlayed: Int) {
    var deck: ListBuffer[Card] = new ListBuffer[Card]

    def addToDeck(card: Card): Unit = {
        deck += card
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
            companies(i) = new Team(applications, playerLocation, playerScore, playerPermanentDailyRoutineCards, playerPermanentArchitectureStudyCards)
        }
        val myTeam = companies(0)
        val ennemyTeam = companies(1)

        // --- CARDS --- //
        val cardLocationsCount = readLine.toInt
        for(i <- 0 until cardLocationsCount) {
            // cardsLocation: the location of the card list. It can be HAND, DRAW, DISCARD or OPPONENT_CARDS (AUTOMATED and OPPONENT_AUTOMATED will appear in later leagues)
            val Array(cardsLocation, _trainingCardsCount, _codingCardsCount, _dailyRoutineCardsCount, _taskPrioritizationCardsCount, _architectureStudyCardsCount, _continuousDeliveryCardsCount, _codeReviewCardsCount, _refactoringCardsCount, _bonusCardsCount, _technicalDebtCardsCount) = readLine split " "
            val trainingCardsCount = _trainingCardsCount.toInt
            val codingCardsCount = _codingCardsCount.toInt
            val dailyRoutineCardsCount = _dailyRoutineCardsCount.toInt
            val taskPrioritizationCardsCount = _taskPrioritizationCardsCount.toInt
            val architectureStudyCardsCount = _architectureStudyCardsCount.toInt
            val continuousDeliveryCardsCount = _continuousDeliveryCardsCount.toInt
            val codeReviewCardsCount = _codeReviewCardsCount.toInt
            val refactoringCardsCount = _refactoringCardsCount.toInt
            val bonusCardsCount = _bonusCardsCount.toInt
            val technicalDebtCardsCount = _technicalDebtCardsCount.toInt
        }

        // --- MOVES --- //
        val possibleMovesCount = readLine.toInt
        for(i <- 0 until possibleMovesCount) {
            val possibleMove = readLine
        }

        gamePhase match {
            case "MOVE"         => println("RANDOM")
            case "RELEASE"      => println("RANDOM")
            case "GIVE_CARD"    => println("RANDOM")
            case "THROW_CARD"   => println("RANDOM")
            case "PLAY_CARD"    => println("RANDOM")
            case _              => 
        }
        

        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
        

        // In the first league: RANDOM | MOVE <zoneId> | RELEASE <applicationId> | WAIT; In later leagues: | GIVE <cardType> | THROW <cardType> | TRAINING | CODING | DAILY_ROUTINE | TASK_PRIORITIZATION <cardTypeToThrow> <cardTypeToTake> | ARCHITECTURE_STUDY | CONTINUOUS_DELIVERY <cardTypeToAutomate> | CODE_REVIEW | REFACTORING;
    }
}