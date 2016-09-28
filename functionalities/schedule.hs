module Main where
-- module Schedule where

import Control.Arrow ((&&&),second)
import System.Process (system)
import System.IO
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec
import TexEditing (Reply, MainList, readI)
import qualified TexEditing as TE

fileName :: FilePath
fileName = "/Users/durand/control/functionalities/schedule.txt"

fADJ :: IO ()
fADJ = fGetJobDone choicesList

main :: IO ()
main = fADJ

choicesList :: MainList
choicesList = [["Update","u"],["Remove","r"],["List","l"]]

fGetJobDone :: MainList -> IO ()
fGetJobDone = ap fChooserIO TE.select

fChooserIO :: MainList -> IO Reply -> IO ()
fChooserIO = (=<<) . flip fChooser

fChooser :: Reply -> MainList -> IO ()
fChooser = ((fProcess . (!!1)) .) . flip (!!) . (subtract 1) . readI

fProcess :: Reply -> IO ()
fProcess = ap ((!!) [elf,const updateList] . fromEnum . (== "u")) id
    where
        elf = ap ((!!) [elelf,const listItem] . fromEnum . (== "l")) id
        elelf = (!!) [action,removeItem] . fromEnum . (== "r")
        action = putStrLn "Please enter the above 3."

updateList :: IO ()
updateList =
    putStrLn "Enter the item you want to add." >>
    getLine >>=
    TE.critcom . return . TE.originalParse >>=
    TE.myWriteFile fileName . concat . intersperse "," . concat


listItem :: IO ()
listItem = (>>= TE.getListDone) . TE.myParser $ fileName

removeItem :: IO ()
removeItem = (>>= getDeleteDone) . TE.myParser $ fileName

deleter :: Reply -> MainList -> IO ()
deleter = (TE.writeMainList fileName .) . (TE.uglyModify .) . TE.removeListItem . readI

deleterIO :: MainList -> IO Reply -> IO ()
deleterIO = (=<<) . flip deleter

getDeleteDone :: MainList -> IO ()
getDeleteDone = ap deleterIO TE.select












