# Green-Circle
[Link to the event](https://www.codingame.com/contests/green-circle).

![alt text](https://static.codingame.com/servlet/fileservlet?id=85391284385554)

![](//cdn.codingame.com/smash-the-code/statement/league_wood_04.png)

This is a **league based** challenge.

For this challenge, multiple leagues for the same game are available. Once you have proven yourself against the first Boss, you will access a higher league and extra rules will be available.  
  
Starter AIs are available in the [Starter Kit](https://github.com/societe-generale/GreenCircle/tree/master/starterAIs). They can help you get started with your own bot.

_Introductory video by **Sebastien and Loïc**: [https://youtu.be/OJxV\_zhICls](https://youtu.be/OJxV_zhICls)_

  
This challenge is inspired from the board game [Samsara](https://okaluda.fr/samsara-le-jeu/)

  Goal
------

The game takes place in a company IT department. Management is organizing a hackathon on the Green IT theme. Two development teams are competing to be the most efficient team. Release the required applications before the opponent team to have more points, but respect the Green IT requirements or you'll get drowned in the Technical Debt.

  Rules
-------

Each player has an IT development team. The game takes place in an office with 8 desks. The hackathon will last many turns. Each turn, the players will play one after the other.

This game is based on **Deck Building**. Each team will have its own deck of cards that will get bigger during the game.

Each team starts the game with 4 **BONUS skill** cards and 4 **Technical Debt** cards. Those cards will be their personal draw pile. And their personal discard pile will be reshuffled into the draw pile when it gets empty.

![Deckbuilding: the player discard pile is reshuffled to create the player's draw pile](https://raw.githubusercontent.com/societe-generale/GreenCircle/master/config/Tuto_DeckBuilding.png)  
The team will get more cards and will lose some during the game.

### The game space (the office)

The office is made of 8 desks. Each desk is dedicated to a specific skill

*   TRAINING (0)
    
*   CODING (1)
    
*   DAILY\_ROUTINE (2)
    
*   TASK\_PRIORITIZATION (3)
    
*   ARCHITECTURE\_STUDY (4)
    
*   CONTINUOUS\_INTEGRATION (5)
    
*   CODE\_REVIEW (6)
    
*   REFACTORING (7)
    

Those desks are numbered from 0 to 7. Each of those desks contains 5 **skill** cards at the beginning of the game.

Due to covid-19, Management enforced a one way path in the office so as not to bump into other people. You must always move in the same direction.

### Applications

Each application needs some skills to be released. Applications are the same for both players. Once an application has been released, it is removed from the game for both players.

Applications will get bigger through the leagues.

A small application needs 3 sets of 2 skills (like 2 **REFACTORING**, 2 **TRAINING** and 2 **CODING**)

A big application needs 2 sets of 4 skills (like 4 **DAILY\_ROUTINE** and 4 **CODE\_REVIEW**)

Each **BONUS skill** card provides one good skill (any of them) and one shoddy skill.

Each specific **skill** card provides 2 good skills (of this specific type) and 2 shoddy skills.

For instance, a **CODING** card provides 2 **CODING** skills and 2 shoddy skills. Those shoddy skills can be used to replace any missing skill, but the quality will not be there.

Each shoddy skill used during an application release will give you a **Technical Debt** card. Those cards are useless. They are only here to slow you down since you can draw some from your draw pile.  
![Examples of how to release an application with various skills](https://raw.githubusercontent.com/societe-generale/GreenCircle/master/config/Tuto_Application.png)

### Turn Description

At the beginning of your turn, your team draws 4 cards at random from their draw pile.

1\. Move

The team begins by moving to another desk (they must leave the desk for the other team).  
They will get one **skill** card from the desk they are moving to (TRAINING, CODE\_REVIEW, REFACTORING, CODING...).  
If the desk is empty (no more cards), the team will get a **BONUS skill** card instead.

2\. Use a skill

Will only appear in later leagues.

3\. Release an application

The team can use their available **skills** in hand to RELEASE an application.  
This phase will only appear if you have enough **skills** to do it.  
Don't forget, if you use shoddy skills, you'll receive **Technical Debt** cards!

  

4\. End of the turn

All cards in hand (**skills** and **Technical Debt**) are discarded.

### Game end

The hackathon stops as soon as one team releases its 5th application.

Beware, since this hackathon is on the theme of Green IT, the referees will be paying close attention to the quality of the last released application.  
The 5th application of each team cannot be released with shoddy skills!

When a team has released 5 applications, the game will end once all teams have played the same amount of turns.

The winner is the team that released the most applications.  
In case of a tie, the winning team will be the one that has the less **Technical Debt** cards.

Victory Conditions

*   You released 5 applications before your opponent.
*   You released more applications than your opponent after **200 game phases.**
*   In case of a tie, you have less **Technical Debt** cards than your opponent.

Defeat Conditions

*   Your program does not provide a valid command in time.

  

### 🐞 Debugging tips

*   Hover over a player, card or card pile to see extra information about it
*   Append text after any command and that text will appear above your player
*   Press the gear icon on the viewer to access extra display options
*   Use the keyboard to control the action: space to play/pause, arrows to step 1 frame at a time

  Game Input/Output
-------------------

Input for One Game Turn

Line 1: gamePhase the name of the current game phase. It can be MOVE, RELEASE  
Line 2: applicationCount for the amount of applications that are still to be released.  
Next applicationCount lines: the word APPLICATION followed by 9 integers, the description of the application to release and the needed skills to release them (APPLICATION applicationId trainingNeeded codingNeeded dailyRoutineNeeded taskPrioritizationNeeded architectureStudyNeeded continuousDeliveryNeeded codeReviewNeeded refactoringNeeded).  
1 line per player: 4 integers (Your data is always given first):

*   location : the desk currently used by the player (-1 in the first turn).

*   score : the amount of released applications.

*   permanentDailyRoutineCards : ignore for this league.

*   permanentArchitectureStudyCards : ignore for this league.

  
Next line: cardLocationsCount for the amount of locations where the players have some cards.  
Next cardLocationsCount lines: the location name followed by 10 integers, the number of cards in this location (cardLocation trainingCardsCount codingCardsCount dailyRoutineCardsCount taskPrioritizationCardsCount architectureStudyCardsCount continuousDeliveryCardsCount codeReviewCardsCount refactoringCardsCount bonusCardsCount technicalDebtCardsCount). Location can be HAND, DRAW, DISCARD or OPPONENT\_CARDS (all the opponent's cards in their hand, draw and discard)  
Next line: possibleMovesCount for the amount of possible actions.  
Next possibleMovesCount lines: a string, one possible action.  

Output for One Game Turn

1 line containing one of the following actions depending on the game phase:

*   RANDOM: the player decides to do one of the possible action at random.

*   WAIT: the player decides not to do any optional action.

*   MOVE zoneId: the player moves to another desk.  
    This action is mandatory and is only available in the MOVE phase.

*   RELEASE applicationId: the player releases an application.  
    This action is optional and is only available in the RELEASE phase.

You may append text to a command to have it displayed in the viewer above your player.  
  
Examples:

*   MOVE 3
*   RELEASE 16
*   WAIT nothing to do...
*   RANDOM got no idea...

Constraints

Response time per turn ≤ 50ms (Doing Green IT means sparing resources)  
Response time for the first turn ≤ 1000ms

![](//cdn.codingame.com/smash-the-code/statement/league_wood_04.png)

What is in store for me in the higher leagues?

Extra rules available in higher leagues are:

*   Players will have the ability to play some skill cards
*   Players will have to give one skill card to their opponent if they move too close to them
*   Half of the applications will need more skills to be released

If you want to know more on Green IT (in French)

_Societe Generale ambitions on Green IT : [https://youtu.be/ZstnO7j1y4c](https://youtu.be/ZstnO7j1y4c)_

Our partnership with INR and our signing of the Sustainable IT Charter : [https://careers.societegenerale.com/green-it-program](https://careers.societegenerale.com/green-it-program)

Our experts Masterclasses in video – to know everything on Green IT

_Masterclass #1 Why doing Sustainable IT? : [https://youtu.be/eLffG8Z0iXU](https://youtu.be/eLffG8Z0iXU)_

_Masterclass #2 E-accessibility stakes for IT : [https://youtu.be/oRA\_CrGxGgw](https://youtu.be/oRA_CrGxGgw)_

_Masterclass #3 Architecture and Green IT : [https://youtu.be/x2fMjGqinLA](https://youtu.be/x2fMjGqinLA)_

_Masterclass #4 Moving to a sustainable IT design : [https://youtu.be/kb2PM7OniRk](https://youtu.be/kb2PM7OniRk)_
