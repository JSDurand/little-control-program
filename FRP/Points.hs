module Points (points) where

import Graphics.Rendering.OpenGL
import Control.Arrow

-- points      :: Int -> [(GLfloat,GLfloat,GLfloat)]
-- points n    =  [ (sin (2*pi*k/n'), cos (2*pi*k/n'), 0) | k <- [1..n'] ]
--    where n' =  fromIntegral n

points :: [(GLfloat,GLfloat,GLfloat)]
points =  map (fillIn . fromIntegral2) $ enumFromTo (negate 2) (2 :: Integer) >>=
    \n -> enumFromTo (negate 2) (2 :: Integer)                    >>=
    \m -> return (n,m)

fillIn :: Num a => (a, a) -> (a, a, a)
fillIn (x,y) = (x,y,0)

fromIntegral2 :: Num a => (Integer, Integer) -> (a, a)
fromIntegral2 =  first fromIntegral . second fromIntegral
