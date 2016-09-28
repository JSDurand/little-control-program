-- module Control where

import Graphics.UI.GLUT
import Data.IORef
import Bindings

main :: IO ()
main =  do
  (_progName, _args)    <- getArgsAndInitialize
  initialDisplayMode    $= [DoubleBuffered]
  _window               <- createWindow "My Own Control Center"
  reshapeCallback       $= Just reshape
  angle                 <- newIORef 0
  delta                 <- newIORef 0.1
  pos                   <- newIORef (0, 0)
  keyboardMouseCallback $= Just (keyboardMouse delta pos)
  idleCallback          $= Just (idle angle delta)
  displayCallback       $= display angle pos
  mainLoop
