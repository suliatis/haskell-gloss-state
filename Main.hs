module Main (main) where

import           Control.Concurrent        (threadDelay)
import           Control.Monad             (sequence, unless)
import           Data.List                 (find)
import           Data.Maybe                (fromMaybe)
import           Graphics.Gloss            (color, pictures, rectangleSolid, translate)
import           Graphics.Gloss.Data.Color (black, white)
import           Graphics.Gloss.Rendering  (Picture, State, displayPicture, initState)
import           Graphics.UI.GLFW          (Key (..), Window, pollEvents, swapBuffers)
import           Player                    (Direction (..), Player (..), initialPlayer, movePlayer)
import           System.Exit               (exitSuccess)
import           Window                    (keyIsPressed, withWindow)

windowWidth, windowHeight :: Int
windowWidth = 640
windowHeight = 480

playerSize :: Float
playerSize = 20

defaultPlayerStep :: Float
defaultPlayerStep = 10

sleepInMicros :: Int
sleepInMicros = 2000

main :: IO ()
main = do
  glossState <- initState
  withWindow windowWidth windowHeight "Haskell Shapes" $ \window -> do
    loop window glossState initialPlayer
    exitSuccess
 where
  loop window glossState player = do
    threadDelay sleepInMicros
    pollEvents
    renderFrame window glossState player
    exit <- keyIsPressed window Key'Escape
    direction <- getDirection window
    let newPlayer = movePlayer direction player defaultPlayerStep
    unless exit $ loop window glossState newPlayer

getDirection :: Window -> IO Direction
getDirection window = do
  directions <- sequence
    [ getDirection' Key'Left Direction'Left
    , getDirection' Key'Down Direction'Down
    , getDirection' Key'Up Direction'Up
    , getDirection' Key'Right Direction'Right
    ]
  let maybeDirection = find actualDirection directions
  return $ fromMaybe Direction'Nope maybeDirection
  where
    getDirection' key dir = do
      b <- keyIsPressed window key
      if b then return dir else return Direction'Nope
    actualDirection Direction'Nope = False
    actualDirection _              = True

renderFrame :: Window -> State -> Player -> IO ()
renderFrame window glossState player = do
  displayPicture (windowWidth, windowHeight) white glossState 1.0 $ pictures
    [ drawPlayer player ]
  swapBuffers window

drawPlayer :: Player -> Picture
drawPlayer (Player (x, y)) =
  translate x y $ color black $ rectangleSolid playerSize playerSize

