import Control.Monad
import System.Exit

import EitherIO
import Hearts.Types
import Hearts.Play(playGame)
import Game(newPlayer)
import Logs(writeGame)
import Deck
import Cards

import safe qualified Player

-- | Test a game of the Player against itself.
test_one_play :: [Player] -> IO Bool
test_one_play players = do
  played <- runEitherIO $ playGame 100 players
  case played of
    Right gr -> writeGame gr >> return True
    -- Beware, you need to capture the error if run headless
    Left e -> putStrLn "" >> print e >> return False

main :: IO ()
main = do
  played <- mapM test_one_play [[p1,p2,p3,p4]]
  if and played
     then exitSuccess
     else exitFailure
  where
    p1 = newPlayer "p1" Player.playCard Player.makeBid
    p2 = newPlayer "p2" Player.playCard Player.makeBid
    p3 = newPlayer "p3" Player.playCard Player.makeBid
    p4 = newPlayer "p4" Player.playCard Player.makeBid
