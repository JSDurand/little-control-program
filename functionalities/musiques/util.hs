module Util where

import Control.Monad
import System.FilePath
import System.Directory


type Names = [String]
type Name = String

gDC :: FilePath -> IO [FilePath]
gDC = ap (fmap . (. filter (flip notElem [".", "..",".DS_Store"])) . map . (</>)) getDirectoryContents

getFiles :: IO [FilePath] -> IO [FilePath]
getFiles = (>>= flip myFilter fileExam)

critList :: [[a] -> [a]]
critList = [const [],id]

myFilter :: [[a]] -> ([a] -> IO Bool) -> IO [[a]]
myFilter = ap (flip . ((>>=) .) . flip ((sequence .) . fmap)) ((return .) . flip (zipWith ((critList !!) . fromEnum)))

fileExam :: FilePath -> IO Bool
fileExam = doesFileExist

dirEx :: FilePath -> IO Bool
dirEx = doesDirectoryExist

toName :: FilePath -> Name
toName = reverse . takeWhile ((/=) '/') . reverse

getFileNames :: FilePath -> IO Names
getFileNames = (>>= return . map toName) . gDC

elimEmpty :: Names -> Names
elimEmpty = filter $ not . null

getOnlyFiles :: FilePath -> IO Names
getOnlyFiles = (>>= return . elimEmpty) . (>>= return . map toName) . getFiles . gDC

readI :: String -> Int
readI = read
