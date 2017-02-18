module GUI
(smallText,
getSquare,
drawGrid,
getMenu,
drawPiece,
draw)
where

import Graphics.Gloss.Interface.IO.Game
import Block
import Constants
import Data.Monoid ((<>))
import Logic
import Grid

draw :: World -> IO Picture
draw (World grid piece score _) = return $
  drawPiece grid piece <>
  drawGrid grid <>
  getMenu score

drawGrid :: Grid -> Picture
drawGrid grid =
  fixed <> (Color black . Pictures $ vertical ++ horizontal)
  where
    (Grid (orx, ory) (gridx, gridy) widthspace heightspace _) = grid
    endx = orx + fromIntegral gridx * widthspace
    endy = ory - fromIntegral gridy * heightspace
    fixed = drawFixedBlocks grid
    vertical = [Line [(orx + fromIntegral i * widthspace, ory), (orx + fromIntegral i * widthspace, endy)] | i <- [0..gridx]]
    horizontal = [Line [(orx, ory - fromIntegral j * heightspace), (endx, ory - fromIntegral j * heightspace)] | j <- [0..gridy]]

getMenu :: Score -> Picture
getMenu score =
  smallText (50, 100) ("Score: " ++ show score)
  -- <> smallText (100, 100) "Tetris"
  -- <> smallText (100, 50) "Next Block:"

smallText :: Point -> String -> Picture
smallText (x,y) = Translate x y . Scale 0.2 0.2 . Text

drawFixedBlocks :: Grid -> Picture
drawFixedBlocks (Grid _ _ _ _ []) =
  Blank
drawFixedBlocks (Grid (orx, ory) (dimx, dimy) w h ((_, _, Nothing):xs)) =
  Blank <> drawFixedBlocks (Grid (orx, ory) (dimx, dimy) w h xs)
drawFixedBlocks (Grid (orx, ory) (dimx, dimy) w h ((x, y, Just pieceColor):xs)) =
  Color pieceColor $
  getSquare (Grid (orx, ory) (dimx, dimy) w h ((x, y, Just pieceColor):xs)) (x, y, Just pieceColor)
  <> drawFixedBlocks (Grid (orx, ory) (dimx, dimy) w h xs)

getSquare :: Grid -> Block -> Picture
getSquare _ (_, _, Nothing) = Blank
getSquare (Grid (orx, ory) _ w h _) (bx, by, Just pieceColor) =
  if outsideGridExtended (bx, by, Just pieceColor)
    then Blank
    else Color pieceColor $
      Translate (orx + w/2 + (fromIntegral bx - 1) * w) (ory - h/2 - (fromIntegral by - 1) * h) $
      rectangleSolid w h

drawPiece :: Grid -> Piece -> Picture
drawPiece grid piece = Pictures . map (getSquare grid) $ blockList piece
