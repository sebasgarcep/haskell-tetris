import GUI
import Graphics.Gloss.Interface.IO.Game
import Grid
import Block
import Logic
import Clock
import Constants
import InputHandler

main :: IO ()
main = do
  newPiece <- getRandomPiece

  playIO
    (InWindow "Tetris" (640, 480) (1,1))
    white
    30
    (World getNewGrid newPiece 0 (Clock tickSpeed))
    draw
    handleInput
    gameLogic
