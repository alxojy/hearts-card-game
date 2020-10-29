# Hearts Card Game
Haskell player implementation for a trick taking playing card game.

The purpose of this assignment was to familiarise us with Haskell and to use functional programming in implementing a player for the Hearts card game. My implementation is in ```staticgame/Player.hs```. Besides that, I have also implemented a naive player and a random player. 

### Player
A heuristic player has been implemented for the game AI. 

### Semantics
A trick consists of 2 cards (for 2P) and 4 cards (for 4P). A round consists of many tricks. A game consists of many rounds. 

### Strategy
My player determines the best move for the current trick based on the information stored in its memory (cards played in previous tricks) to get the unplayed cards. Unplayed cards are cards held by the other opponents in their hand. To obtain this
information, I simply removed the cards in my hand, cards in the previous tricks and cards in the current trick from a full deck. 

### How does it work?
My game AI loops through all the valid cards in its hand that it could play in the current trick and simulates the outcome for the trick. The outcome is the score that the AI could get if it plays a particular card. 1 heart card = 1 pt, Queen of Spades = 13 pts. If the AI has cards following the lead suit, it searches through the unplayed cards and cards in current trick for cards following the lead suit to see if it there is a card with rank higher than mine. This implies that I do not have to take the cards in the current trick (score = 0). If there isn't, it selects the card with the lowest score generated. 

If the AI does not have a card following the lead suit (offsuit), it plays the Queen of Spades (if it has it in hand)/ (Ace of Spades/King of Spades) if the Queen of Spades haven't been played as these 2 cards are higher than SQ and can be a liability. Else, it plays the highest Heart card (if it has it). Otherwise, it plays the highest card it has in hand. 
If the AI is required to be the first player in the trick, it returns the lowest ranked card with the lowest score simulated (while abiding to the breaking rule). To ensure that it abides the breaking rule, the function heartsBroken searches through previously played cards to check if a Heart card has been played. 

To abide to the rules, there are 3 main function which determines which card/cards is/are valid to play:
1. noBleed - obeys the no bleeding rule for the first trick of the round. 
2. notFirstPlayer - follows the lead suit if the AI has cards of this suit in hand. else, all cards in hand are valid to play. 
3. firstPlayer - plays a non-heart card if hearts are not broken (obeys breaking rule). if hearts are broken, all cards in hand are valid to play. 

The playCard function determines which function to call based on the memory (to see if it's the first trick) and if there are cards in the current trick. 

The functions in the code have been sorted based on their functionality. Small reusable functions have been created for higher order functions to call them. Furthermore, the AI handles cases for both 2P and 4P. There are also function compositions used. 

### Memory usage
The cards played in the previous tricks is stored in the memory in the form ie "H2 SQ" separated by a whitespace. Then to get the cards, the function words is used. 

### How to run the game?
```stack run```

This would run a single game and the results will be displayed on the terminal.

I scored a HD for this assignment.




