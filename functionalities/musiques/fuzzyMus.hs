module Main where
-- module FuzzyMus where

import System.Directory
import System.FilePath
import Util (dirEx, getOnlyFiles, Names, getFileNames, toName, Name, readI)
import Listen (dir, select, Reply, execution, ynChoice)
import Text.Fuzzy as F
import Data.Char (isDigit)
import Control.Applicative ((<*>), (<$>), liftA2)
import Control.Monad (join)

ffilter :: String -> [String] -> String -> String -> (String -> String) -> Bool -> [Fuzzy String String]
ffilter = F.filter

mainf :: IO Names
mainf = putStrLn "Enter search term: " >>
  getLine >>=
  \term -> traverseFiles dir >>=
  \namesList -> return . map original $ ffilter term namesList "" "" id False
  
main :: IO ()
main = do
  ls <- mainf
  let names = map toName ls
  reply <- select . return . take pageSize $ names
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
      else putStrLn ("Input a number from 1 to " ++ show pageSize) >> selectAgain
    else case reply of
      "e" -> return ("exit", "no directory")
      "n" -> do
        rep <- select . return . take pageSize . drop (pageSize * pageNo) $ names
        pageDealing names rep (pageNo + 1) oriLs 
      "p" -> do
        rep <- select . return . take pageSize . drop (pageSize * (pageNo - 2)) $ names
        if pageNo > 1 then pageDealing names rep (pageNo - 1) oriLs
          else pageDealing names rep 1 oriLs
      _   -> putStrLn "Input n, e or p only" >> selectAgain
  where
  realReply = (names ++ ["exit"]) !! realIndex
  realDirec = toDir $ oriLs !! realIndex
  realIndex = pageSize * (pageNo - 1) + readI reply - 1
  selectAgain = (select . return . take pageSize . drop (pageSize * (pageNo - 1))$ names) >>=
    \newReply -> pageDealing names newReply pageNo oriLs

ending :: IO ()
ending = putStrLn "Want to search again?" >>
    ynChoice >>= (!!) [return (),main] . fromEnum . (== "1")

