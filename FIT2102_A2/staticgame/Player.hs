-- 30101670

{-
Report:

A heuristic player has been implemented for the game AI. 

Semantics:
A trick consists of 2 cards (for 2P) and 4 cards (for 4P). A round consists of many tricks. A game consists of many rounds. 

Strategy:
My player determines the best move for the current trick based on the information stored in its memory (cards played in previous tricks) to get 
the unplayed cards. Unplayed cards are cards held by the other opponents in their hand. To obtain this information, I simply removed the cards in my 
hand, cards in the previous tricks and cards in the current trick from a full deck. 

How does it work?
My game AI loops through all the valid cards in its hand that it could play in the current trick and simulates the outcome for the trick. 
The outcome is the score that the AI could get if it plays a particular card. 1 heart card = 1 pt, Queen of Spades = 13 pts. 
If the AI has cards following the lead suit, it searches through the unplayed cards and cards in current trick for cards following the lead suit 
to see if it there is a card with rank higher than mine. This implies that I do not have to take the cards in the current trick (score = 0). 
If there isn't, it selects the card with the lowest score generated. 

If the AI does not have a card following the lead suit (offsuit), it plays the Queen of Spades (if it has it in hand)/ (Ace of Spades/King of Spades)
if the Queen of Spades haven't been played as these 2 cards are higher than SQ and can be a liability. Else, it plays the highest Heart card (if it has
it). Otherwise, it plays the highest card it has in hand. 

If the AI is required to be the first player in the trick, it returns the lowest ranked card with the lowest score simulated (while abiding to the
breaking rule). To ensure that it abides the breaking rule, the function heartsBroken searches through previously played cards to check if a Heart 
card has been played. 

To abide to the rules, there are 3 main function which determines which card/cards is/are valid to play:
1. noBleed - obeys the no bleeding rule for the first trick of the round. 
2. notFirstPlayer - follows the lead suit if the AI has cards of this suit in hand. else, all cards in hand are valid to play. 
3. firstPlayer - plays a non-heart card if hearts are not broken (obeys breaking rule). if hearts are broken, all cards in hand are valid to play. 

The playCard function determines which function to call based on the memory (to see if it's the first trick) and if there are cards in the current trick. 

The functions in the code have been sorted based on their functionality. Small reusable functions have been created for higher order functions to call
them. Furthermore, the AI handles cases for both 2P and 4P. There are also function compositions used. 

Memory usage:
The cards played in the previous tricks is stored in the memory in the form ie "H2 SQ" separated by a whitespace. Then to get the cards, the function 
words is used. 
-}

module Player (
    playCard,
    makeBid
)
where

-- | Imports
import Hearts.Types
import Cards
import Deck
import Data.List
import Data.Ord (comparing)

-- | Getters

-- get the suit of the card
getSuit :: Card -> Suit
getSuit (Card suit _) = suit

-- get the rank of the card
getRank :: Card -> Rank
getRank (Card _ rank) = rank 

-- get the lead suit in the current trick
getLeadSuit :: [(Card, PlayerId)] -> Suit
getLeadSuit tr = getSuit (last $ map fst tr)

-- get unplayed cards aka cards haven't played in previous tricks, not in player's hand and not in the current trick
getUnplayedCards :: 
    [Card] -> -- cards in hand
    [(Card,PlayerId)] -> -- cards in trick
    Maybe ([(Card, PlayerId)], String) -> -- memory
    [Card]
getUnplayedCards cinhand cintrick prev = sortedDeck \\ (cinhand ++ getPlayedCards cintrick prev) 

-- get played cards aka cards in previous tricks and in the current trick
getPlayedCards :: 
    [(Card,PlayerId)] -> -- cards in trick
    Maybe ([(Card, PlayerId)], String) -> -- memory
    [Card]
getPlayedCards cintrick prev = map fst cintrick ++ cardsInMemory prev -- append the cards in current trick and previous tricks

-- | Filtering

-- filter cards in hand that are not of suit Hearts
notHeartCards :: [Card] -> [Card]
notHeartCards cards = filter (\x -> getSuit(x) /= Heart) cards

-- filter cards in hand that are not of suit Hearts and not Queen of Spades
notPointCards :: [Card] -> [Card]
notPointCards cards = filter (\x -> x /= (Card Spade Queen)) $ notHeartCards cards

-- filter the player's cards based on the suit specified
cardsOfSuit :: [Card] -> Suit -> [Card]
cardsOfSuit cards suit = filter (\x -> getSuit x == suit) cards

-- | Sorting

-- quick sort the ranks of the player's cards
-- inspired by https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
cardRankSort :: [Card] -> [Card]
cardRankSort [] = [] -- base case
cardRankSort (pivot:rest) = (cardRankSort lesser) ++ [pivot] ++ (cardRankSort greater)
    where
        lesser = filter (\x -> getRank x < getRank pivot) rest -- cards smaller than pivot, go left
        greater = filter (\x -> getRank x >= getRank pivot) rest -- cards larger than or equal to pivot, go right

-- | Checking for condition(s)

-- check if contains Two of Clubs in hand
containsTwoOfClubs :: [Card] -> Bool
containsTwoOfClubs cards = elem (Card Club Two) cards

-- check if Hearts has already been played in previous trick/ in current trick (heart broken rule)
heartsBroken :: [(Card,PlayerId)] -> Maybe([(Card, PlayerId)], String) -> Bool
heartsBroken cintrick prev = not $ null $ filter (\x -> getSuit(x) == Heart) $ getPlayedCards cintrick prev

-- Queen of Spades or Hearts in current trick
pointCardsInTrick :: [(Card,PlayerId)] -> Bool
pointCardsInTrick cintrick = not $ null $ (map fst $ cintrick) \\ (notPointCards $ map fst $ cintrick)

-- | Memory

-- parser for the memory string
-- memory: cards played in previous tricks
-- string is in the form ie "H2 SQ"
cardsInMemory :: Maybe([(Card, PlayerId)], String) -> [Card] 
cardsInMemory (Nothing) = [] -- base case
cardsInMemory (Just a) = (map read $ words $ snd a :: [Card]) ++ (map fst $ fst a) -- cards in memory ++ cards in previous trick

-- store all the previous cards played in memory
-- cards are stored in a string. string is in the form ie "H2 SQ" separated by white space
toMemory :: Maybe([(Card, PlayerId)], String) -> String
toMemory Nothing = ""
toMemory (Just a) = (unwords $ map show $ map fst $ fst a) ++ " " ++ (snd a) -- cards in memory ++ cards in previous trick

-- | Simulation 

-- returns a list of cards with the lowest score sorted from lowest to highest rank
simulation :: 
    [Card] -> -- valid cards
    [Card] -> -- cards in hand
    [(Card,PlayerId)] -> -- cards in trick
    Maybe ([(Card, PlayerId)], String) -> -- memory
    [Card]
simulation cvalid cinhand cintrick prev 
    -- gets the simulation score for each card and return the minimum cards sorted by rank
    | not $ null cintrick = cardRankSort $ map snd $ minElements $ map (\x -> simScore x cit unplayed (getLeadSuit cintrick)) cvalid 
    | otherwise = cardRankSort $ map snd $ minElements $ map (\x -> simScore x cit unplayed (getSuit x)) cvalid 
    where
        unplayed = getUnplayedCards cinhand cintrick prev -- unplayed cards
        cit = map fst cintrick -- get cards in trick
        
-- returns the score along with the card        
-- if there is an unplayed card with rank > the rank of the card in hand, score = 0
-- else, calculate the score
simScore :: 
    Card -> -- card in hand to simulate
    [Card] -> -- cards in trick
    [Card] -> -- unplayed cards
    Suit -> -- lead suit
    (Int, Card) -- (score, card)
simScore cv cit unplayed ls 
    | (not $ null v) && getRank cv < getRank (last $ cardRankSort $ v) = (0, cv) -- has unplayed card of rank higher than my card. return (0, card)
    | otherwise = (hpt + qspt, cv) -- return (score, card)
    where
        v = cardsOfSuit (unplayed ++ cit) ls -- append cards in trick and unplayed cards
        hpt = length $ filter (\x -> getSuit(x) == Heart) (unplayed ++ cit) -- length (n) of heart cards = n pts
        qspt
            | not $ null $ filter (\x -> x == (Card Spade Queen)) (unplayed ++ cit) = 13 -- SQ = 13 pts
            | otherwise = 0 

-- gets a list of tuples with the smallest first tuple element
-- ie. [(1,'a'),(2,'b'),(3,'c'),(1,'d')] returns [(1,'a'),(1,'d')]
minElements :: Ord a => [(a, b)] -> [(a, b)]
minElements [] = []
minElements arr = filter ((==) (minimum (map fst arr)) . fst) arr

-- choose the highest card lower than the highest card in the trick
-- if this isn't present, choose the highest
ducking :: 
    [Card] -> -- valid cards in hand
    [(Card,PlayerId)] -> -- cards in trick
    Card
ducking cvalid cintrick 
    | pointCardsInTrick cintrick && getRank(last sortedvc) < getRank(last sortedTricks) = last sortedvc -- if there are
    -- point cards in the current trick, return the highest card lower than the highest card following the lead suit
    -- in trick
    | otherwise = head sortedvc -- return the highest ranked card following lead suit
      where 
        sortedTricks = cardRankSort $ cardsOfSuit (map fst cintrick) (getLeadSuit cintrick) -- sort cards following lead suit
        -- in current trick
        sortedvc = cardRankSort cvalid -- sort valid cards in hand

-- | Main functions that abide by the rules in selecting valid cards for the game

-- called in the first trick to ensure it follows no bleeding rule
-- has card following lead suit? - yes - ducking
--                               - no  - play SA/SK if present
--                                     - has non-point cards? - yes - play highest non-point card
--                                                            - no  - play largest (heart) card
noBleed :: 
    [Card] -> -- cards in hand
    [(Card,PlayerId)] -> -- cards in trick
    Card
noBleed cinhand cintrick
    | not $ null npc = ducking npc cintrick -- got lead suit, ducking if point cards in trick
    | elem (Card Spade Ace) cinhand = (Card Spade Ace) -- no lead suit, play SA if present
    | elem (Card Spade King) cinhand = (Card Spade King) -- no lead suit, play SK if present
    | not $ null $ nfl =  last nfl -- no lead, play highest non-heart
    | otherwise = last $ cardRankSort cinhand -- play highest card in hand
    where
        npc = cardsOfSuit cinhand $ getLeadSuit cintrick -- cards in hand that are following the lead suit
        nfl = cardRankSort $ notPointCards $ cinhand -- non-point cards in hand and do not follow lead suit

-- this function is called when the player isn't the lead player in the trick. if the player has a card following the lead suit, 
-- the AI will call the simulation function to determine the best card to play. 
-- else, it uses heuristics to determine the next card to play if it doesn't have a card following the lead suit. 
-- the AI will play SQ (if present in hand)/ play SA or SK if SQ hasn't been played yet as these 2 cards are higher than SQ and can
-- be a liability to the AI who might end up taking the SQ/ highest heart card in hand (if the AI has heart cards)/ highest card
-- in hand
notFirstPlayer :: 
    [Card] -> -- cards in hand
    [(Card,PlayerId)] -> -- cards in trick
    Maybe ([(Card, PlayerId)], String) -> -- memory
    Card
notFirstPlayer cinhand cintrick prev
    | not $ null hasSuit = head $ simulation hasSuit cinhand cintrick prev -- got card(s) following lead suit
    | elem (Card Spade Queen) cinhand = (Card Spade Queen) -- no cards following lead suit, play SQ if present
    | elem (Card Spade Queen) unplayed && elem (Card Spade Ace) cinhand = (Card Spade Ace) -- no cards following lead suit, 
    -- play SA if present and SQ hasn't been played yet
    | elem (Card Spade Queen) unplayed && elem (Card Spade King) cinhand = (Card Spade King) -- no cards following lead suit, 
    -- play SK if present and SQ hasn't been played yet
    | not $ null hearts = last hearts -- play highest Heart card
    | otherwise = last $ cardRankSort cinhand -- play highest card
    where
        hasSuit = cardsOfSuit cinhand $ getLeadSuit cintrick -- cards following the lead suit
        unplayed = getUnplayedCards cinhand cintrick prev -- unplayed cards
        hearts = cardRankSort $ cardsOfSuit cinhand Heart -- cards of Heart suit in hand

-- this function is called when the player is the first player in the trick. if hearts hasn't been broken, the player will play
-- a non-heart card (if possible). else, it plays any card in hand
-- it calls the simulation function to simulate which is the best card to play. then, it selects the best card with the lowest rank
firstPlayer :: 
    [Card] -> -- cards in hand
    [(Card,PlayerId)] -> -- cards in trick
    Maybe ([(Card, PlayerId)], String) -> -- memory
    Card
firstPlayer cinhand cintrick prev
    | heartsBroken cintrick prev = head $ simulation cinhand cinhand cintrick prev -- hearts are broken, can play any card
    | not $ null (notHeartCards cinhand) = head $ simulation (notHeartCards cinhand) cinhand cintrick prev -- hearts are not broken, only
    -- non - heart cards
    | otherwise = head $ simulation cinhand cinhand cintrick prev -- any card in hand

-- | Returns a card and the memory to be stored in every trick

playCard :: PlayFunc
playCard _ cinhand cintrick prev
    | prev == Nothing && cintrick== [] && containsTwoOfClubs cinhand = ((Card Club Two), toMemory prev) -- start with C2
    | prev == Nothing = (noBleed cinhand cintrick, toMemory prev) -- first trick. must abide no breaking rule
    | null cintrick = (firstPlayer cinhand cintrick prev, toMemory prev) -- first player in the trick
    | otherwise = (notFirstPlayer cinhand cintrick prev, toMemory prev) -- not the first player in the trick

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined