module Main (main) where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (sequence, unless)
import           Control.Monad.State.Strict (StateT, get, lift, put, runStateT)
import           Data.List                  (find)
import           Data.Maybe                 (fromMaybe)
import           Graphics.Gloss             (color, pictures, rectangleSolid, translate)
import           Graphics.Gloss.Data.Color  (black, white)
import           Graphics.Gloss.Rendering   (Picture, State, displayPicture, initState)
import           Graphics.UI.GLFW           (Key (..), Window, pollEvents, swapBuffers)
import           Player                     (Direction (..), Player (..), defaultStep, initialPlayer, movePlayer)
import           Playground                 (Playground (..), defualtPlayground, isOutside)
import           System.Exit                (exitSuccess)
import           Window                     (keyIsPressed, withWindow)

playground :: Playground
playground = defualtPlayground

defaultPlayerStep :: Float
defaultPlayerStep = 10

sleepInMicros :: Int
sleepInMicros = 2000

main :: IO ()
main = do
  glossState <- initState
  withWindow (width playground) (height playground) "Haskell State" $ \window -> do
    runStateT (loop window glossState) initialPlayer
    exitSuccess

loop :: Window -> State -> StateT Player IO ()
loop window glossState = do
    lift $ threadDelay sleepInMicros
    lift $ pollEvents
    player <- get
    direction <- lift $ getDirection window
    let player' = movePlayer direction player defaultStep
    if isOutside playground player'
       then put player
       else put player'
    lift $ renderFrame window glossState player
    exit <- lift $ keyIsPressed window Key'Escape
    unless exit $ loop window glossState

getDirection :: Window -> IO Direction
getDirection window = do
  directions <- sequence
    [ getDirection' Key'Left Direction'Left
    , getDirection' Key'H Direction'Left
    , getDirection' Key'A Direction'Left
    , getDirection' Key'Down Direction'Down
    , getDirection' Key'J Direction'Down
    , getDirection' Key'S Direction'Down
    , getDirection' Key'Up Direction'Up
    , getDirection' Key'K Direction'Up
    , getDirection' Key'W Direction'Up
    , getDirection' Key'Right Direction'Right
    , getDirection' Key'L Direction'Right
    , getDirection' Key'D Direction'Right
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
  displayPicture (width playground, height playground) white glossState 1.0 $ pictures
    [ drawPlayer player ]
  swapBuffers window

drawPlayer :: Player -> Picture
drawPlayer (Player (x, y) size) =
  translate x y $ color black $ rectangleSolid size size

