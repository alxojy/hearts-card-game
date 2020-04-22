{-# LANGUAGE OverloadedStrings #-}
module Logs(
  writeGame,
  clearLog,
  clearAllLogs,
  )
where

import Data.Time.Clock.System (systemSeconds, getSystemTime)
import Data.Csv
import Data.List (sort, sortBy, elemIndex)
import System.FilePath
import System.Directory (createDirectoryIfMissing,
                         removeDirectoryRecursive,
                         doesDirectoryExist)
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V

import Cards
import Hearts.Types

instance ToField HandScore where
  toField = toField . score

instance ToField Suit where
  toField = toField . show

log_dir :: FilePath
log_dir = "logs/"

playerDir :: PlayerId -> FilePath
playerDir pid = log_dir </> pid

logFile :: PlayerId -> String -> IO FilePath
logFile playerId stamp = do
  createDirectoryIfMissing True player_dir
  return $ player_dir </> stamp <.> "csv"
  where
    playerName = takeBaseName playerId
    player_dir = playerDir playerName

foldStr :: (a -> String) -> String -> [a] -> String
foldStr _ _ [] = ""
foldStr f sep (x: xs) = foldl (\ys y -> ys ++ sep ++ f y) (f x) xs

fromCards :: [Card] -> String
fromCards cards = foldStr toId "," cards
  where
    toId (Card suit rank) = show suit ++ r
      where
        r | rank < Jack = show (fromEnum rank + 2)
          | otherwise = take 1 (show rank)

toCards :: [Trick] -> String
toCards tricks = foldStr getCards ";" tricks
  where
    getCards = fromCards . map first

fromScores :: PlayerId -> [HandScore] -> Int
fromScores pid scores = (score . head) $
  filter (\HandScore{playerId} -> playerId == pid) scores

clearLog :: Player -> IO ()
clearLog Player{playerId=p} = do
  exist <- doesDirectoryExist player_dir
  if exist
    then removeDirectoryRecursive player_dir
    else return ()
  where
    player_dir = (playerDir p)

clearAllLogs :: [Player] -> IO ()
clearAllLogs players = mapM_ clearLog players

writeHand :: [Player] -> HandResult -> IO ()
writeHand players (HandResult tricks scores) = do
  time <- getSystemTime
  let stamp = systemSeconds time
      row player result = [
        (stamp,
          elemIndex player pids,
          fromScores player scores,
          start,
          toCards ordered)]
  mapM_ (\(p, q) -> logFile p (show stamp) >>= \f ->
            BL.appendFile f (encode (row p q))) (zip pids points)
  where
    -- Extract player ids and sort them
    pids =  (sort . map (\Player{playerId} -> playerId)) players
    points = sortBy (\HandScore{playerId=p} HandScore{playerId=q} -> compare p q) scores
    ordered = (reverse . map match) tricks
    match = sortBy (\x y -> compare (second x) (second y))
    start = elemIndex (((\Player{playerId} -> playerId) . head) players) pids

writeGame :: GameResult -> IO ()
writeGame (GameResult plays _ players) =
  mapM_ (writeHand players) (reverse plays)
