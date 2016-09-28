module Main where
-- module Update where

import Util
import Listen (ynSelect, dir, Reply, Choices)
import Control.Monad
import System.Process (system)
import System.FilePath
import System.Directory

-- Ask the directory name

adn :: IO Name
adn = putStrLn "What is the name of the directory?" >> getLine

isDir :: Name -> IO Bool
isDir = dirEx . (dir </>)

handler :: IO Names -> IO ()
handler = ap ((=<<) . (. fromEnum) . (!!) . (ap ((++) . (return . crMv)) (return . mv))) (>>= isDir . (!!0))

-- Ask want to update what song

upSong :: FilePath -> IO Name
upSong = ap (liftM2 renderer . ynSelect) id . getOnlyFiles

renderer :: Reply -> Names -> Name
renderer = flip (!!) . subtract 1 . readI

-- Updating

crMv :: IO Names -> IO ()
crMv = (=<<) $ ap ((>>) . createDirectory . (dir </>) . (!!0)) (ap (renameFile . (dir </>) . (!!1)) ((dir </>) . ap ((</>) . (!!0)) (!!1)))

mv :: IO Names -> IO ()
mv = (=<<) $ ap (renameFile . (dir </>) . (!!1)) ((dir </>) . ap ((</>) . (!!0)) (!!1))

-- Combining all

joiner :: Name -> Name -> Names
joiner = (. return) . ((++) . return)

mainf :: FilePath -> IO ()
mainf = handler . liftM2 joiner adn . upSong

main :: IO ()
main = mainf dir
