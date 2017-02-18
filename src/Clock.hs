module Clock
(Clock(Clock),
restartClock,
isOver,
tick)
where

newtype Clock = Clock Int deriving (Show)

restartClock :: Int -> Clock -> Clock
restartClock x (Clock 0) = Clock x
restartClock _ clock = clock

isOver :: Clock -> Bool
isOver (Clock 0) = True
isOver _ = False

tick :: Clock -> Clock
tick (Clock 0) = Clock 0
tick (Clock x) = Clock (x-1)
