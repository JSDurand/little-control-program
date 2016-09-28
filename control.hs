module Main where

import System.IO
import Control.Monad
import Control.Monad.State
import System.Process (system)

type Command = String

basicFormat = " "
formatingString1 = "          "
formatingString2 = "          "
separator = (>>=) [1..80] . const $ return '-'
ydlc = "/Users/durand/Desktop/Centre/Musique/commands/youtube-dl-shell.command"
ltmc = "/Users/durand/control/functionalities/musiques/listen"
fmc  = "/Users/durand/control/functionalities/musiques/fuzzyMus"
udmc = "/Users/durand/control/functionalities/musiques/update"
rdrc = "/Users/durand/Desktop/Centre/Reminder/Reminder.org"
roc = "/Users/durand/romans/.stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/romans/romans"
poc = "/Users/durand/Desktop/Centre/Autres/a"
sdc = "/Users/durand/control/functionalities/schedule"
woc = "networksetup -setairportpower en0 on"
wfc = "networksetup -setairportpower en0 off"
fimc = "/usr/bin/find \"/Users/durand/Music/iTunes/iTunes Media/Music/\" -iname "
fpmc = " -execdir mpv --loop=inf {} \\;"
emc = "/usr/local/Cellar/emacs/24.5/bin/emacs "
mlc = "/Users/durand/control/functionalities/texEditing"

list :: [String]
list = [pr1, pr2, "Listen: l", "Update: u", "Download: d", "Reminder: r", "Exit: e", "Romans: ro", "htop: h", "wifi on/off: wo/wf", "subilme text: su", "safari: sf", "mpsyt: mp", "Fuzzy Musique: f", "fim: fi", "fpm: fp" ]

to_fill1 :: [String]
to_fill1 = ["","","command line: cl","Main List: ml","Last Command: !","Videos: v", "Schedule: s"]

list1 :: [String]
list1 =  (++) to_fill1 rest_list1

rest_list1 :: [String]
rest_list1 = rest_func list to_fill1

rest_func :: [String] -> [String] -> [String]
rest_func = flip flip (const (return ([]))) . (((>>=) . enumFromTo 1) .) . (. length) . (-) . length

tr_func :: String -> String -> String
tr_func = ((++) .) =<< flip (++) . concat . (>> return basicFormat) . enumFromTo 1 . (-) 34 . length

tr_list :: [String]
tr_list = zipWith tr_func list list1

myPrint = putStrLn . (++) formatingString1

smoothy :: IO a -> IO ()
smoothy = flip (>>) . return $ ()

myExec :: String -> IO ()
myExec = smoothy . system

myExec2 :: String -> String -> IO ()
myExec2 = ((smoothy . system . (uncurry (++))) .) . (,)

musique_proc :: (String, String) -> IO ()
musique_proc = \(s1,s2) -> do
    myPrint "Enter search keyword:"
    putStr formatingString2
    hFlush stdout
    name <- getLine
    let f = (. (("\"" ++ name ++ "*\"") ++)) . myExec2
        in f s1 s2

pr1 = "Choose a function."

pr2 = "The correspondence follows."

pending :: IO String
pending = myPrint "Enter anything to continue:" >>
    putStr formatingString2 >>
    hFlush stdout >>
    getLine

expla :: Int -> IO ()
expla = (>> (system "tput clear" >> sequence_ (map myPrint tr_list))) . decision
    where
        decision = (!!) [pending, return ""] . fromEnum . (== 0)
-- expla x = (decision x >>) $
--     (system "tput clear" >>) $
--     sequence_ $ map myPrint tr_list
--         where
--             decision x = if x == 0 then return "" else pending

io :: IO a -> StateT Command IO a
io = liftIO

main :: IO ()
main = evalStateT (mainf 0) "None"

mainf :: Int -> StateT Command IO ()
mainf x = (io . expla $ x) >>=
    \_ -> (io . putStrLn $ separator) >>=
    \_ -> get >>= 
    io . lastPrinter >>=
    \_ -> (io . putStr $ formatingString2) >>=
    \_ -> (io . hFlush $ stdout) >>=
    \_ -> io getLine >>= process
    where 
        process ans = case ans of
            "l" -> (io . myExec $ ltmc) >> 
                put ans >> mainf 0
            "d" -> (io . myExec $ ydlc) >>
                put ans >> mainf 0
            "u" -> (io . myExec $ udmc) >>
                put ans >> mainf 0
            "r" -> (io . myExec2 emc $ rdrc) >> 
                put ans >> mainf 0
            "ro" -> (io . myExec $ "open -a safari \"http://localhost:3000/romans\"") >>
                (io . myExec $ "cd /Users/durand/romans && " ++ roc) >>
                put ans >> mainf 1
            "h" -> (io . system $ "sudo htop") >> 
                put ans >> mainf 0
            "wo" -> (io . system $ woc) >> 
                put ans >> mainf 0
            "wf" -> (io . system $ wfc) >> 
                put ans >> mainf 0
            "su" -> (io . system $ "subl") >> 
                put ans >> mainf 0
            "f" -> (io . system $ fmc) >> 
                put ans >> mainf 0
            "sf" -> (io . system $ "open -a safari") >> 
                put ans >> mainf 0
            "fi" -> (io . musique_proc $ (fimc, "")) >> 
                put ans >> mainf 0
            "fp" -> (io . musique_proc $ (fimc, fpmc)) >> 
                put ans >> mainf 0
            "mp" -> (io . system $ "mpsyt") >> 
                put ans >> mainf 0
            "cl" -> do
                (io $ myPrint "Enter command:" >>
                            putStr formatingString2 >>
                            hFlush stdout >>
                            getLine >>= system)
                put ans
                mainf 1
            "ml" -> io (myExec mlc) >> 
                put ans >> mainf 1
            "!" -> get >>= process
            "v" -> (io . system $ "open " ++ poc) >> put ans >> mainf 0
            "s" -> (io . system $ sdc) >> put ans >> mainf 1
            "e" -> put ans >> return ()
            _ -> (io . putStrLn $ "What is this?") >> mainf 1

lastPrinter :: Command -> IO ()
lastPrinter = (>> putStrLn separator) . putStrLn . ("Last: " ++)



-- Archive

-- ltmc = "/Users/durand/Desktop/Centre/Musique/commands/listen_to_musique.command"

-- udmc = "/Users/durand/Desktop/Centre/Musique/commands/update-musique.command"

-- ppc = "/Users/durand/Desktop/Centre/Romans/Parsing.py"

-- list = [pr1, pr2, "Listen: l", "Update: u", "Download: d", "Reminder: r", "Exit: e", "Romans: ro", "htop: h", "wifi on/off: wo/wf", "subilme text: su", "safari: sf", "mpsyt: mp", "open an application: o", "fim: fi", "fpm: fp" ]
