-- 30101670

{-
Report:

Random player
First round - yes - has C2? - yes - play C2
                  - has card following lead suit? - yes - play random card following lead suit
--                                                - no  - has non-point cards? - yes - play random non-point cards
--                                                                             - no  - play any random card

First player - yes - hearts broken - yes - play any random card 
                                   - no  - play any random non-heart cards
             - no  - have suit of first card - yes - play random card following suit
                                             - no  - play any random card

It uses a rng to get a random card. 

How to achieve randomness?
It uses the length of valid cards as a seed for the rng. 
-}

module RandomPlayer (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Hearts.Types
import Cards
import System.Random

-- random number generator
rng :: Int -> Int -> (Int,StdGen)
rng upper g = randomR (0, upper-1) $ mkStdGen g

-- getters
-- get the suit of the card
getSuit :: Card -> Suit
getSuit (Card suit _) = suit

-- get the lead suit in the current trick
getLeadSuit :: [(Card, PlayerId)] -> Suit
getLeadSuit tr = getSuit (last $ map fst tr)

-- filter the player's cards based on the suit
cardsOfSuit :: [Card] -> Suit -> [Card]
cardsOfSuit [] _ = []
cardsOfSuit cards suit = filter (\x -> getSuit x == suit) cards

-- contains Two of Clubs in hand
containsTwoOfClubs :: [Card] -> Bool
containsTwoOfClubs cards = elem (Card Club Two) cards

-- filter cards in hand that are not Hearts
notHeartCards :: [Card] -> [Card]
notHeartCards [] = []
notHeartCards cards = filter (\x -> getSuit(x) /= Heart) cards

-- filter cards in hand that are not Hearts
notPointCards :: [Card] -> [Card]
notPointCards [] = []
notPointCards cards = filter (\x -> x /= (Card Spade Queen)) $ notHeartCards cards

-- check if Hearts has already been played in previous round
heartsBroken :: Maybe([(Card, PlayerId)], String) -> Bool
heartsBroken (Nothing) = False
heartsBroken (Just a) = not $ null $ filter (\x -> getSuit(x) == Heart) $ map fst $ fst a

-- get the "seed" for the random number generator
getSeed :: Maybe([(Card, PlayerId)], String) -> Int
getSeed (Nothing) = 943857234
getSeed (Just a) = read $ snd a :: Int

-- get a random card & a new seed which is saved to memory
-- uses the random number generator
getRandCard :: [Card] -> Maybe ([(Card, PlayerId)], String) -> (Card, String)
getRandCard cards prev = (cards !! ran, show ran)
    where
        ran = fst $ rng (length cards) (getSeed prev)

-- called in the first round to ensure it follows no bleeding rule
-- has card following lead suit? - yes - play random card following lead suit
--                               - no  - has non-point cards? - yes - play random non-heart cards
--                                                            - no  - play any random card
noBleed :: [Card] -> [(Card,PlayerId)] -> Maybe ([(Card, PlayerId)], String) -> (Card, String)
noBleed cinhand cintrick prev
    | not $ null npc = getRandCard npc prev
    | not $ null nfl = getRandCard nfl prev
    | otherwise = getRandCard cinhand prev
    where
        npc = cardsOfSuit cinhand $ getLeadSuit cintrick
        nfl = notPointCards $ cinhand
        
-- not the first player
-- have suit of first card? - yes - play random card following suit
--                          - no  - play any random card
retCardGotLead :: [Card] -> [(Card,PlayerId)] -> Maybe ([(Card, PlayerId)], String) -> (Card, String)
retCardGotLead cinhand cintrick prev
    | not $ null hasSuit = getRandCard hasSuit prev
    | otherwise = getRandCard cinhand prev
    where
        hasSuit = cardsOfSuit cinhand $ getLeadSuit cintrick

-- first player - no leading card
-- hearts broken - yes - play any random card 
--               - no  - play any random non-heart cards
retCardNoLead :: [Card] -> Maybe ([(Card, PlayerId)], String) -> (Card, String)
retCardNoLead cinhand prev
    | heartsBroken prev = getRandCard cinhand prev -- get memory to check if hearts broken
    | not $ null notHearts = getRandCard notHearts prev -- play a non Heart card
    | otherwise = getRandCard cinhand prev
    where
        notHearts = notHeartCards cinhand

playCard :: PlayFunc
playCard _ cinhand cintrick prev
    | prev == Nothing && cintrick == [] && containsTwoOfClubs cinhand = ((Card Club Two), "101755602") -- initialise seed
    | prev == Nothing = (noBleed cinhand cintrick prev)
    | null cintrick = (retCardNoLead cinhand prev)
    | otherwise = (retCardGotLead cinhand cintrick prev)

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined