module Player
  ( Player(..)
  , Direction(..)
  , initialPlayer
  , movePlayer
  )
where

import           Graphics.Gloss (Point)

data Player = Player { position :: Point }

initialPlayer :: Player
initialPlayer = Player(0,0)

data Direction
  = Direction'Left
  | Direction'Down
  | Direction'Up
  | Direction'Right
  | Direction'Nope

movePlayer :: Direction -> Player -> Float -> Player
movePlayer direction (Player(x, y)) step =
    case direction of
      Direction'Left  -> Player(x - step, y)
      Direction'Down  -> Player(x, y - step)
      Direction'Up    -> Player(x, y + step)
      Direction'Right -> Player(x + step, y)
      Direction'Nope  -> Player(x, y)

