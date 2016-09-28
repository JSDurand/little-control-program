-- This file serves mainly to study how to transform ordinary style Haskell to point-free ones.

import System.Process (system)
import System.IO

smoothy :: IO a -> IO ()
smoothy = (>> return ())

myExec2 :: String -> String -> IO ()
myExec2 = ((smoothy . system . (uncurry (++))) .) . (,)

---------------------------------------------------------------------------------
-- original

formatingString2 = "          "

musique_proc = \(s1,s2) -> do
    print "Enter search keyword:"
    putStr formatingString2
    name <- getLine
    let f = (. (("\"" ++ name ++ "*\"") ++)) . myExec2
        in f s1 s2

-- I insert something here in Vi IMproved. 

---------------------------------------------------------------------------------
func = ((++) .) =<< flip (++) . foldl (++) ([]) . (>> return " ") . enumFromTo 1 . (-) 34 . length

func2 :: [String] -> [String] -> [String]
func2 = flip flip (const (return ([]))) . (((>>=) . enumFromTo 1) .) . (. length) . (-) . length
