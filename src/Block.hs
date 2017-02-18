module Block
(blockList,
shapeColor,
getRandomPiece,
blockCollision)
where

import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Maybe
import Constants

afterRotation :: Orientation -> [Block] -> [Block]
afterRotation North = id
afterRotation East = rotate90
afterRotation South = rotate90 . rotate90
afterRotation West = rotate90 . rotate90 . rotate90

rotate90 :: [Block] -> [Block]
rotate90 (center:list) = map (rotateWithRespectTo center) (center:list)
  where
    rotateWithRespectTo :: Block -> Block -> Block
    rotateWithRespectTo (cx, cy, pieceColor) (bx, by, _) = (-ory + cx, orx + cy, pieceColor)
      where
        (orx, ory) = (bx - cx, by - cy)

blockList :: Piece -> [Block]
blockList (Piece shape (cx, cy, pieceColor) orientation) = afterRotation orientation $ listResult shape
  where
    listResult :: Shape -> [Block]
    listResult shapeObj =
      case shapeObj of
        O -> [(cx, cy, pieceColor), (cx + 1, cy, pieceColor), (cx, cy + 1, pieceColor), (cx + 1, cy + 1, pieceColor)]
        T -> [(cx, cy, pieceColor), (cx + 1, cy, pieceColor), (cx, cy - 1, pieceColor), (cx - 1, cy, pieceColor)]
        S -> [(cx, cy, pieceColor), (cx - 1, cy, pieceColor), (cx, cy + 1, pieceColor), (cx + 1, cy + 1, pieceColor)]
        Z -> [(cx, cy, pieceColor), (cx + 1, cy, pieceColor), (cx, cy + 1, pieceColor), (cx - 1, cy + 1, pieceColor)]
        I -> [(cx, cy, pieceColor), (cx, cy + 1, pieceColor), (cx, cy - 1, pieceColor), (cx, cy + 2, pieceColor)]
        L -> [(cx, cy, pieceColor), (cx, cy + 1, pieceColor), (cx, cy - 1, pieceColor), (cx + 1, cy - 1, pieceColor)]
        J -> [(cx, cy, pieceColor), (cx, cy + 1, pieceColor), (cx, cy - 1, pieceColor), (cx - 1, cy - 1, pieceColor)]

shapeColor :: Shape -> Color
shapeColor shape =
  case shape of
    O -> yellow
    T -> red
    S -> green
    Z -> blue
    I -> orange
    L -> cyan
    J -> magenta

getCenter :: Shape -> Block
getCenter shape = (5, -2, Just $ shapeColor shape)

getRandomPiece :: IO Piece
getRandomPiece = do
  shape <- getStdRandom (randomR (minBound :: Shape, maxBound :: Shape))
  return (Piece shape (getCenter shape) North)

blockCollision :: Block -> Block -> Bool
blockCollision (x,y,val1) (i,j,val2) = isJust val1 && isJust val2 && i == x && j == y
