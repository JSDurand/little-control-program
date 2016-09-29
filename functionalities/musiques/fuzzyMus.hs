module Main where
-- module FuzzyMus where

import System.Directory
import System.FilePath
import System.Process (system)
import System.IO (hFlush, stdout)
import Util (dirEx, getOnlyFiles, Names, getFileNames, toName, Name, readI)
import Listen (dir, select, Reply, execution, ynChoice, Choices)
import Text.Fuzzy as F
import Data.Char (isDigit)
import Control.Applicative ((<*>), (<$>), liftA2)
import Control.Monad (join, ap)

ffilter :: String -> [String] -> String -> String -> (String -> String) -> Bool -> [Fuzzy String String]
ffilter = F.filter

mainf :: IO Names
mainf = system "tput clear" >>
  putStrLn "Enter search term: " >>
  getLine >>=
  \term -> traverseFiles dir >>=
  \namesList -> return . map original $ ffilter term namesList "" "" id False
  
main :: IO ()
main = do
  ls <- mainf
  let names = map toName ls
  reply <- selectPage 1 . return . take pageSize $ names
  (rep, direc) <- pageDealing names reply 1 ls
  execution rep direc
  ending

toDir :: FilePath -> Name
toDir =  reverse . dropWhile ('/' /=) . reverse

pageSize :: Int
pageSize =  10

traverseFiles :: FilePath -> IO Names
traverseFiles direc = (>>= (\ls -> fmap ((map (direc </>) $ fst ls) ++) (recurse direc $ snd ls))) . (liftA2 (,) <$> getOnlyFiles <*> getFileNames) $ direc
  where
  helper :: FilePath -> FilePath -> IO [FilePath]
  helper p y = dirEx (p </> y) >>= \result -> if result then traverseFiles (p </> y) else return []
  recurse :: FilePath -> [FilePath] -> IO [FilePath]
  recurse fp = fmap join . mapM (helper fp)

pageDealing :: Names -> Reply -> Int -> Names -> IO (Reply, Name)
pageDealing names reply pageNo oriLs = if isDigit (head reply)
    then if readI reply <= pageSize
      then return (realReply, realDirec)
      else system "tput clear" >>
        putStrLn ("Input a number from 1 to " ++ show pageSize) >> selectAgain
    else case reply of
      "e" -> return ("exit", "no directory")
      "n" -> do
        system "tput clear"
        rep <- selectPage (pageNo + 1) . return . take pageSize . drop (pageSize * pageNo) $ names
        pageDealing names rep (pageNo + 1) oriLs 
      "p" -> do
        system "tput clear"
        rep <- selectPage savePrev . return . take pageSize . drop (pageSize * (pageNo - 2)) $ names
        pageDealing names rep savePrev oriLs
      ('g' : xs) -> do
        system "tput clear"
        let nom = readI xs
        rep <- selectPage nom . return . take pageSize . drop (pageSize * (nom - 1)) $ names
        pageDealing names rep nom oriLs
      "h" -> do
        system "tput clear"
        rep <- selectPage 1 . return . take pageSize $ names
        pageDealing names rep 1 oriLs
      "l" -> do
        system "tput clear"
        rep <- selectPage lastPageNo . return . take pageSize . drop (pageSize * (lastPageNo - 1)) $ names
        pageDealing names rep lastPageNo oriLs
      _   -> do
        system "tput clear"
        putStrLn "Input n, e, p, l, h, or g only"
        selectAgain
  where
  savePrev    = if pageNo > 1 then pageNo - 1 else 1
  lastPageNo  = let (q, r) = divMod (length names) pageSize in q + 1 - fromEnum (r == 0)
  realReply   = (names ++ ["exit"]) !! realIndex
  realDirec   = toDir $ oriLs !! realIndex
  realIndex   = pageSize * (pageNo - 1) + readI reply - 1
  selectAgain = (selectPage pageNo . return . take pageSize . drop (pageSize * (pageNo - 1)) $ names) >>=
    \newReply -> pageDealing names newReply pageNo oriLs

ending :: IO ()
ending = putStrLn "Want to search again?" >>
    ynChoice >>= (!!) [return (),main] . fromEnum . (== "1")

separator = (>>=) [1..80] . const $ return '-'

converter :: Names -> Choices
converter = ap (ap (const $ zipWith (ap (const (++)) ((++ ") ") . show))) (enumFromTo 1 . length)) id

selectPage   :: Int -> IO Names -> IO Reply
selectPage n =  flip (>>) getReply . proprePrint n

proprePrint   :: Int -> IO Names -> IO ()
proprePrint n =  flip (>>) (putStrLn separator >> putStrLn (show n) >> putStrLn separator) . (=<<) (sequence_ . map putStrLn . converter . (++ ["exit"]))

getReply :: IO Reply
getReply = putStr "#? " >> hFlush stdout >> getLine

