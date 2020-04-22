module Main where

import Hearts.Play
import Hearts.Types
import Game
import EitherIO
import Logs
import safe qualified Player
import Control.Monad

-- This sets up a tournament with four instances of your player playing against
-- each other.  You can run different players against each other, but you'll
-- need to change the Module names of those players (don't forget to change the
-- module name back to "Player" when you submit your code)
players :: [Player]
players =
    [ newPlayer "Player1" Player.playCard Player.makeBid
    , newPlayer "Player2" Player.playCard Player.makeBid
    , newPlayer "Player3" Player.playCard Player.makeBid
    , newPlayer "Player4" Player.playCard Player.makeBid
    ]

main :: IO ()
main = do
  clearAllLogs players
  played <- runEitherIO $ playGame 100 players
  case played of
    Right gr@(GameResult hr scores _) -> do
      forM_ hr print
      putStrLn "=============="
      forM_ scores print
      writeGame gr
    Left e -> print e
