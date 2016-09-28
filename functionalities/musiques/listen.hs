-- module Main where
module Listen where

import System.Directory
import System.Random
import System.FilePath
import System.Process (system)
import System.IO
import Control.Monad
import Util

-- This is adapted from a shell script I wrote before.

-- First list all files in the directory, the data part.

type Reply = String
type Choices = [String]

dir :: FilePath
dir = "/Users/durand/Desktop/Centre/Musique/Chansons/"

-- Then define a function to perform selection and choice. This is the input part.

converter :: Names -> Choices
converter = ap (ap (const $ zipWith (ap (const (++)) ((++ ") ") . show))) (enumFromTo 1 . length)) id

select :: IO Names -> IO Reply
select = flip (>>) getReply . proprePrint

proprePrint :: IO Names -> IO ()
proprePrint = (=<<) (sequence_ . map putStrLn . converter . (++ ["exit"]))

getReply :: IO Reply
getReply = putStr "#? " >> hFlush stdout >> getLine

-- A randomised selection
rSelect :: IO Names -> IO Reply
rSelect = fmap show . flip (>>=) (getStdRandom . randomR . (,) 1 . length)

-- A processing function, the processor part.

loop :: FilePath -> IO Names -> IO Reply -> IO (Bool, FilePath, FilePath)
loop = (flip ((>>=) . (fmap readI)) .) . flip . loop_proc
    where
        loop_proc :: FilePath -> Int -> IO Names -> IO (Bool, FilePath, FilePath)
        loop_proc fp rep x = do
            list_item <- x
            fileP     <- return ((!!) (list_item ++ ["exit"]) . subtract 1 $ rep)
            bl        <- dirEx (fp </> fileP)
            return (bl, fp, fileP)

execution :: String -> String -> IO ()
execution x y = if x == "exit" then return () else (>> return ()) . system $ ("mpv --loop=inf \"" ++ y </> x ++ "\"")

mainf :: FilePath -> IO ()
mainf = (>>= looping) . (ap (ap loop getFileNames) (select . getFileNames))
    where
        looping (bl,fp,fileP) = case bl of
            True -> mainf (fp </> fileP)
            False -> flip execution fp fileP

ranMainf :: FilePath -> IO ()
ranMainf = (>>= looping) . (ap (ap loop getFileNames) (rSelect . getFileNames))
    where
        looping (bl,fp,fileP) = case bl of
            True -> ranMainf (fp </> fileP)
            False -> flip execution fp fileP

main :: IO ()
main = putStrLn "Want a random one?" >>
    ynChoice >>= (!!) [ranMainf dir,mainf dir] . fromEnum . (== "2") >>
    ending

ending :: IO ()
ending = putStrLn "Want to pick again?" >>
    ynChoice >>= (!!) [return (),main] . fromEnum . (== "1")

ynChoice :: IO Reply
ynChoice = ynSelect . return $ ["y","n"]

ynSelect :: IO Names -> IO Reply
ynSelect = flip (>>) getReply . ynPrint

ynPrint :: IO Names -> IO ()
ynPrint = (=<<) (sequence_ . map putStrLn . converter)
