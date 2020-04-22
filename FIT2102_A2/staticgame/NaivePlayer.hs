-- 30101670

{- 
Report:

The naive player doesn't use memory. 
If it has a card following the lead suit, it returns the lowest card following the lead suit. In the case when it doesn't 
have a card following the lead suit, it will play the lowest non-heart card unless it only has hearts left. 
If it leads, it will select a non-heart card unless it only has hearts left. 
-}

module NaivePlayer (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Hearts.Types
import Cards

-- getters
-- get the suit of the card
getSuit :: Card -> Suit
getSuit (Card suit _) = suit

-- get the rank of the card
getRank :: Card -> Rank
getRank (Card _ rank) = rank 

-- get the lead suit in the current trick
getLeadSuit :: [(Card, PlayerId)] -> Suit
getLeadSuit tr = getSuit (last $ map fst tr)

-- filter the player's cards based on the suit
cardsOfSuit :: [Card] -> Suit -> [Card]
cardsOfSuit [] _ = []
cardsOfSuit cards suit = filter (\x -> getSuit x == suit) cards

-- quick sort the ranks of the player's cards
-- inspired by https://wiki.haskell.org/Introduction#Quicksort_in_Haskell
cardRankSort :: [Card] -> [Card]
cardRankSort [] = []
cardRankSort (pivot:rest) = (cardRankSort lesser) ++ [pivot] ++ (cardRankSort greater)
    where
        lesser = filter (\x -> getRank x < getRank pivot) rest
        greater = filter (\x -> getRank x >= getRank pivot) rest

-- contains Two of Clubs in hand
containsTwoOfClubs :: [Card] -> Bool
containsTwoOfClubs cards = elem (Card Club Two) cards

-- choose the lowest Club to lead
-- function is called at the start of the game when the player has Two of Clubs in hand
startLeading :: [Card] -> Card
startLeading cinhand = head $ cardRankSort $ cardsOfSuit cinhand Club

-- filter cards in hand that are not Hearts
notHeartCards :: [Card] -> [Card]
notHeartCards [] = []
notHeartCards cards = filter (\x -> getSuit(x) /= Heart) cards

-- not the first player
-- play the lowest card following the lead suit (if possible). else, play the lowest non heart card.
-- else, play the lowest card remaining in hand
retCardGotLead :: [Card] -> [(Card,PlayerId)] -> Card
retCardGotLead cinhand cintrick 
    | not $ null hasSuit = head $ cardRankSort $ hasSuit
    | length notHearts > 0 = head $ cardRankSort $ notHearts
    | otherwise = head $ cinhand
    where
        hasSuit = cardsOfSuit cinhand $ getLeadSuit cintrick
        notHearts = notHeartCards cinhand

-- plays a non-heart card (if possible). else, play the lowest card in hand
retCardNoLead :: [Card] -> Card
retCardNoLead cinhand 
    | length notHearts > 0 = head $ cardRankSort $ notHearts
    | otherwise = head $ cinhand
    where
        notHearts = notHeartCards cinhand

{-
type PlayFunc
  =  PlayerId -- ^ this player's Id so they can identify themselves in the bids
              -- and tricks
  -> [Card]   -- ^ the player's cards
  -> [(Card, PlayerId)]                 -- ^ cards in the current trick, so far
  -> Maybe ([(Card, PlayerId)], String) -- ^ previous player's state
  -> (Card, String)             -- ^ the player's chosen card and new state
  -}
playCard :: PlayFunc
playCard _ cinhand cintrick prev
    | prev == Nothing && cintrick == [] && containsTwoOfClubs cinhand = (startLeading cinhand, "")
    | not $ null cintrick = (retCardGotLead cinhand cintrick, "")
    | otherwise = (retCardNoLead cinhand, "")

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
