{-# LANGUAGE OverloadedStrings #-}
-- module Romans where
module Main where

import System.Directory
import TexEditing (Reply, Choices, MainList, smoothy, select, readI)
import Network.Wreq
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Lens
import Text.HTML.TagSoup
import Data.Char
import Control.Monad
import System.Process (system)
import Data.List
import Data.Maybe

-- Internet connection part, provided by a super programmer

-- match \d+\.html
isNumberHtml :: LBS.ByteString -> Bool
isNumberHtml = (".html" ==) . LBS.dropWhile isDigit

wanted :: Tag LBS.ByteString -> Bool
wanted = ap ((&&) . isTagOpenName "a") (isNumberHtml . fromAttrib "href")

wurl :: String
wurl = "http://www.piaotian.net/html/7/7430/"

ficPath :: String
ficPath = "/Users/durand/Desktop/Centre/Romans/nombre.txt"

bodify :: Response LBS.ByteString -> LBS.ByteString
bodify = (^. responseBody)

hrefsFunc :: LBS.ByteString -> [String]
hrefsFunc = map (LBS.unpack . fromAttrib "href") . filter wanted . parseTags

mainf :: String -> IO ()
mainf = ap ((=<<) . ((readFile ficPath >>=) .) . func) ((>>= return . hrefsFunc . bodify) . get)
    where
        func urlX h n = case n == show (length h) of
            True -> putStrLn "No updates yet"
            False -> execution urlX h (readI n) >>
                (writeFile (ficPath ++ " - temp") . show . length $ h) >>
                removeFile ficPath >>
                renameFile (ficPath ++ " - temp") ficPath

appending :: String -> [String] -> Int -> String
appending = (.(!!)) . (.) . (++) . (++) "open -a safari "

execution :: String -> [String] -> Int -> IO ()
execution = (((smoothy . system) .) .) . appending

-- Data structure part, similar to TexEditing
choicesList :: MainList
choicesList = [["Walk into saince", wurl]]

fProcess :: Choices -> IO ()
fProcess = mainf . (!!1)

fChooser :: Reply -> MainList -> IO ()
fChooser = (fProcess .) . flip (!!) . subtract 1 . readI

fChooserIO :: MainList -> IO Reply -> IO ()
fChooserIO = (=<<) . flip fChooser

fGetJobDone :: MainList -> IO ()
fGetJobDone = ap fChooserIO select

fAskDoJob :: IO ()
fAskDoJob = fGetJobDone choicesList

main :: IO ()
main = putStrLn "Choose a novel:" >> putStrLn "" >> fAskDoJob

-- Now we have to simulate updates if the novel is already finished, but I haven't read it. Maybe I can write a local website and update by myself, >w<.
-- This is possible: we just write html files and open it in browser. Maybe it is time to learn some elm. Just maybe...
-- But this would require to run a website when using the function; I don't think this is a good idea.
-- I shall make a novel-control program instead. Or maybe just keep a list of progresses of the novels, and update each time I read a small fragrance of it.

-- Archive

-- mainf :: String -> IO ()
-- mainf urlX = do
--     r <- get urlX
--     let body = r ^. responseBody :: LBS.ByteString
--         tags = parseTags body
--         links = filter wanted tags
--         hrefs = map (LBS.unpack . fromAttrib "href") links
--     nombre <- readFile ficPath
--     case nombre == show (length hrefs) of
--         True -> putStrLn "No updates yet"
--         False -> do 
--             execution urlX hrefs
--             writeFile (ficPath ++ " - temp") . show . length $ hrefs
--             removeFile ficPath
--             renameFile (ficPath ++ " - temp") ficPath
  -- forM_ hrefs LBS.putStrLn

  -- return . length $ links
  
    -- system $ "open -a safari " ++ url ++ hrefs!!(length hrefs - 1)

-- tagify :: LBS.ByteString -> [Tag LBS.ByteString]
-- tagify = parseTags

-- isNumberHtml lbs = (LBS.dropWhile isDigit lbs) == ".html"

-- wanted t = isTagOpenName "a" t && isNumberHtml (fromAttrib "href" t)

-- appending :: String -> [String] -> String
-- appending = (. (ap (!!) (subtract 1 . length))) . (++) . ("open -a safari " ++)

-- findElem :: Eq a => a -> [a] -> Int
-- findElem = (fromJust .) . elemIndex


