module Constants
(gridHeight,
gridWidth,
Piece(Piece),
Block,
Center,
Shape(O,T,S,Z,I,L,J),
Orientation(North, South, East, West),
Grid(Grid),
tickSpeed)
where

import System.Random
import Graphics.Gloss.Interface.IO.Game

gridWidth :: Int
gridWidth = 10

gridHeight :: Int
gridHeight = 15

tickSpeed :: Int
tickSpeed = 12

data Shape = O | T | S | Z | I | L | J deriving (Bounded, Enum)
instance Random Shape where
  random g = case randomR (fromEnum (minBound :: Shape), fromEnum (maxBound :: Shape)) g of
                 (r, g') -> (toEnum r, g')
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                         (r, g') -> (toEnum r, g')

data Orientation = North | South | East | West
type Block = (Int, Int, Maybe Color)
type Center = Block
data Piece = Piece Shape Center Orientation

type Dimension = (Int, Int)
data Grid = Grid Point Dimension Float Float [Block]
