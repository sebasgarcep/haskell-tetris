module Logic
(gameLogic,
Score,
World(World))
where

import Grid
import Block
import Clock
import Constants
import Data.Maybe

type Score = Int
data World = World Grid Piece Score Clock

gameLogic :: Float -> World -> IO World
gameLogic _ = gameTick

newPieceAddedLogic :: World -> World
newPieceAddedLogic (World grid piece score clock) =
  World newGrid piece (addScore + score) clock
  where
    (newGrid, addScore) = removeFullLines grid

removeFullLines :: Grid -> (Grid, Score)
removeFullLines grid =
  foldl (\n f -> f n) (grid, 0) functionList
  where
    functionList = [removeOneLine n | n <- [1..gridHeight]]

removeOneLine :: Int -> (Grid, Score) -> (Grid, Score)
removeOneLine n (Grid point dim w h list, score) =
  if isFull row
    then (Grid point dim w h replacementBlockList, score + 100)
    else (Grid point dim w h list, score)
  where
    row :: [Block]
    row = filter (\(_, j, _) -> j == n) list
    isFull :: [Block] -> Bool
    isFull = all (\(_, _, val) -> isJust val)
    wipe :: [Block] -> [Block]
    wipe = filter (\(_,y,_) -> y /= n)
    pullDown :: [Block] -> [Block]
    pullDown = map (\(x,y,val) -> if y < n then (x,y+1,val) else (x,y,val))
    replacementBlockList :: [Block]
    replacementBlockList = [(i,1,Nothing) | i <- [1..gridWidth]] ++ pullDown (wipe list)

gameTick :: World -> IO World
gameTick (World grid piece score (Clock 0)) =
  if moveAllowed grid $ moveDown piece
    then return $ World grid (tryDrop grid piece) score (Clock tickSpeed)
    else do
      newPiece <- getRandomPiece
      return . newPieceAddedLogic $ World (fixPiece grid piece) newPiece score (Clock tickSpeed)

gameTick (World grid piece score (Clock t)) =
  if moveAllowed grid piece
    then return $ World grid piece score (Clock $ t-1)
    else do
      newPiece <- getRandomPiece
      return . newPieceAddedLogic $ World (fixPiece grid piece) newPiece score (Clock t)
