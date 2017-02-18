module Grid
(getNewGrid,
fixPiece,
pieceGridCollision,
tryDrop,
tryMoveLeft,
tryMoveRight,
flipCW,
flipCCW,
moveAllowed,
moveDown,
outsideGrid,
outsideGridExtended,
replaceBlock)
where

import Constants
import Block

getNewGrid :: Grid
getNewGrid = Grid
              (startx, starty)
              (gridWidth, gridHeight)
              tileSize
              tileSize
              [(i, j, Nothing) | i <- [1..gridWidth], j <- [1..gridHeight]]
  where
    startx = -300
    starty = 230
    tileSize = 30

fixPiece :: Grid -> Piece -> Grid
fixPiece (Grid a b c d list) piece = Grid a b c d (map (replaceBlock $ blockList piece) list)

replaceBlock :: [Block] -> Block -> Block
replaceBlock checkfor (x,y,val) =
  let
    match = filter (\(i,j,_) -> i == x && j == y) checkfor
    in if (\r -> r /= []) match then head match else (x,y,val)

pieceGridCollision :: Grid -> Piece -> Bool
pieceGridCollision (Grid _ _ _ _ list) piece = or booleanlist
  where
    booleanlist :: [Bool]
    booleanlist =
      do
        pieceBlock <- blockList piece
        gridBlock <- list
        return $ blockCollision pieceBlock gridBlock

flipCW :: Grid -> Piece -> Piece
flipCW = flipAny nextOr
  where
    nextOr :: Orientation -> Orientation
    nextOr North = East
    nextOr East = South
    nextOr South = West
    nextOr West = North

flipCCW :: Grid -> Piece -> Piece
flipCCW = flipAny nextOr
  where
    nextOr :: Orientation -> Orientation
    nextOr North = West
    nextOr West = South
    nextOr South = East
    nextOr East = North

flipAny :: (Orientation -> Orientation) -> Grid-> Piece -> Piece
flipAny _ _ (Piece O center orientation) = Piece O center orientation
flipAny nextOr grid (Piece shape center orientation) =
  if moveAllowed grid nextPiece
    then nextPiece
    else Piece shape center orientation
  where
    nextPiece :: Piece
    nextPiece = Piece shape center (nextOr orientation)

outsideGrid :: Block -> Bool
outsideGrid (x,y,_) = x < 1 || x > gridWidth || y > gridHeight

outsideGridExtended :: Block -> Bool
outsideGridExtended (x, y, val) = outsideGrid (x, y, val) || y < 1

outOfBounds :: Piece -> Bool
outOfBounds piece = any outsideGrid $ blockList piece

moveAllowed :: Grid -> Piece -> Bool
moveAllowed grid piece = not $ outOfBounds piece || pieceGridCollision grid piece

moveAny :: (Piece -> Piece) -> Grid -> Piece -> Piece
moveAny moveFunction grid piece =
  if moveAllowed grid nextPiece
    then nextPiece
    else piece
  where
    nextPiece = moveFunction piece

tryMoveLeft :: Grid -> Piece -> Piece
tryMoveLeft = moveAny moveLeft

tryMoveRight :: Grid -> Piece -> Piece
tryMoveRight = moveAny moveRight

tryDrop :: Grid -> Piece -> Piece
tryDrop = moveAny moveDown

moveDown :: Piece -> Piece
moveDown (Piece shape (cx, cy, val) orientation) = Piece shape (cx, cy + 1, val) orientation

moveLeft :: Piece -> Piece
moveLeft (Piece shape (cx, cy, val) orientation) = Piece shape (cx - 1, cy, val) orientation

moveRight :: Piece -> Piece
moveRight (Piece shape (cx, cy, val) orientation) = Piece shape (cx + 1, cy, val) orientation
