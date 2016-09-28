module Display (idle, display) where

import Graphics.UI.GLUT
import Control.Monad
import Data.IORef
import Points
import Cube

display :: IORef GLfloat -> IORef (GLfloat, GLfloat) -> DisplayCallback
display angle pos = do 
  clear [ColorBuffer]
  loadIdentity
  (x',y') <- get pos
  translate $ Vector3 x' y' 0
  preservingMatrix $ do
    rotate (45 :: GLfloat) $ Vector3 0 0 1
    scale 0.4 0.4 (0.4::GLfloat)
    forM_ points $ \(x,y,z) -> preservingMatrix $ do
      color $ Color3 ((x/4)+1) ((y/4)+1) ((z/4)+1)
      translate $ Vector3 x y z
      cube 0.1
  swapBuffers

idle             :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta =  do
  postRedisplay Nothing

