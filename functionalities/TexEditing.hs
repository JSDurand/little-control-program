-- module Main where
module TexEditing where

import Control.Arrow ((&&&),second)
import System.Process (system)
import System.IO
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
-- First this should hold a main working list so that I can choose to continue the last working progress.

type Reply = String

type Choices = [String]

type MainList = [[String]]

fileName :: FilePath
fileName = "/Users/durand/control/functionalities/workingList.txt"

fChooser :: Reply -> MainList -> IO ()
fChooser = ((fProcess . (!!1)) .) . flip (!!) . (subtract 1) . readI

fChooserIO :: MainList -> IO Reply -> IO ()
fChooserIO = (=<<) . flip fChooser

fGetJobDone :: MainList -> IO ()
fGetJobDone = ap fChooserIO select

fAskDoJob :: IO ()
fAskDoJob = fGetJobDone choicesList

main = fAskDoJob

choicesList :: MainList
choicesList = [["Execution","e"],["Update","u"],["Remove","r"],["List","l"]]

fProcess :: Reply -> IO ()
fProcess x = case x of 
    "e" -> askDoJob
    "u" -> updateList
    "r" -> removeItem
    "l" -> listItem
    _ -> putStrLn "Please enter the above 4."

-- Parser support

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

originalParse :: String -> Either ParseError MainList
originalParse = liftM return . parse line "(unknown)"

parseCSV :: FilePath -> IO (Either ParseError MainList)
parseCSV = (>>= return . parse csvFile "(unknown)") . readFile

critcom :: IO (Either ParseError MainList) -> IO MainList
critcom = fmap etToMb

etToMb :: Either ParseError MainList -> MainList
etToMb y = case y of
    Right x -> x
    Left x -> [[show x]]

myParser :: FilePath -> IO MainList
myParser = critcom . parseCSV

-- Functionalities

converter :: MainList -> Choices
converter = ap (ap (const $ zipWith (ap (const (++)) ((++ ") ") . show))) (enumFromTo 1 . length)) (map head)

select :: MainList -> IO Reply
select = flip (>>) getReply . proprePrint

proprePrint :: MainList -> IO ()
proprePrint = sequence_ . map putStrLn . converter

getReply :: IO Reply
getReply = putStr "#? " >> hFlush stdout >> getLine

readI :: String -> Int
readI = read

myReadFile :: FilePath -> IO ()
myReadFile x = case x of
    "None" -> putStrLn "You chose none."
    otherwise -> smoothy . system $ "open \"" ++ x ++ "\""

smoothy :: IO a -> IO ()
smoothy = flip (>>) . return $ ()

chooser :: Reply -> MainList -> IO ()
chooser = ((myReadFile . (!!1)) .) . flip (!!) . (flip (-) 1) . readI

chooserIO :: MainList -> IO Reply -> IO ()
chooserIO = (=<<) . flip chooser

getJobDone :: MainList -> IO ()
getJobDone = ap chooserIO select

askDoJob :: IO ()
askDoJob = (>>= getJobDone) . myParser $ fileName

-- It remains to add an "update" function, and to be able to be called from Main.

updateList :: IO ()
updateList =
    putStrLn "Enter the item you want to add." >>
    getLine >>=
    critcom . return . originalParse >>=
    myWriteFile fileName . concat . intersperse "," . concat

removeItem :: IO ()
removeItem = (>>= getDeleteDone) . myParser $ fileName

listItem :: IO ()
listItem = (>>= getListDone) . myParser $ fileName

listIO :: MainList -> IO Reply -> IO ()
listIO = (((>> (putStrLn "Enter anything to end display" >> getLine >> return ())) . proprePrint) .) . const

getListDone :: MainList -> IO ()
getListDone = ap listIO select

myWriteFile :: FilePath -> String -> IO ()
myWriteFile = (. (++ "\n")) . appendFile

writeMainList :: FilePath -> MainList -> IO () 
writeMainList = (. concat . concat . intersperse ["\n"] . map (intersperse ",")) . writeFile 

removeListItem :: Int -> [a] -> [a]
removeListItem = (uncurry (++) .) . ap ((&&&) . take . subtract 1) drop

deleter :: Reply -> MainList -> IO ()
deleter = (writeMainList fileName .) . (uglyModify .) . removeListItem . readI

uglyModify :: MainList -> MainList
uglyModify = uncurry (++) . second (f) . ap (flip $ ap ((&&&) . take) drop) (subtract 1 . length)
    where 
        f = liftM2 (++) (take =<< subtract 1 . length) (g . ap (!!) (subtract 1 . length))
            where 
                g y = case y!!(subtract 1 . length $ y) of
                    "\n" -> return y
                    otherwise -> return $ take (subtract 1 . length $ y) y ++ return (y!!(subtract 1 . length $ y) ++ "\n")

deleterIO :: MainList -> IO Reply -> IO ()
deleterIO = (=<<) . flip deleter

getDeleteDone :: MainList -> IO ()
getDeleteDone = ap deleterIO select






