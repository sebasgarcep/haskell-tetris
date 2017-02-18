module InputHandler
(handleInput)
where

import Logic
import Grid
import Graphics.Gloss.Interface.IO.Game

handleInput :: Event -> World -> IO World
handleInput (EventKey (SpecialKey KeySpace) Up _ _) (World grid piece score clock) =
  return $ World grid (flipCW grid piece) score clock
handleInput (EventKey (SpecialKey KeyEnter) Up _ _) (World grid piece score clock) =
  return $ World grid (flipCCW grid piece) score clock
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) (World grid piece score clock) =
  return $ World grid (tryMoveLeft grid piece) score clock
handleInput (EventKey (SpecialKey KeyRight) Up _ _) (World grid piece score clock) =
  return $ World grid (tryMoveRight grid piece) score clock
handleInput (EventKey (SpecialKey KeyDown) Up _ _) (World grid piece score clock) =
  return $ World grid (tryDrop grid piece) score clock
handleInput _ state = return state
