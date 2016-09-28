-- module Main where
module RomansPre where

-- import TexEditing
import Network.HTTP
import Network.HTTP.Client                     -- http-client
import Network.HTTP.Types.Status (statusCode)
import qualified Data.Text.Encoding as T       -- text
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.ICU.Convert as ICU  -- text-icu
import qualified Data.Text.ICU as ICU
import qualified Data.ByteString.Lazy as BL
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Control.Arrow
import Control.Monad.State
-- import qualified Data.ByteString.UTF8 as DU
-- import qualified Data.Text.Internal as TI
-- import Text.HTML.TagSoup

-- We can skip asking for different actions; we just need to list all novels in queue, and then check for the updates, if that novel is still not complete; if the novel is completed but I am still reading, then just open the last page.
-- The last feature is the most critical part.

-- Internet connection part
-- How can I get html content or parse html content using Haskell?

openURL :: String -> IO String
openURL = (>>= getResponseBody) . simpleHTTP . getRequest

display :: String -> IO ()
display = (>>= putStrLn) . openURL

downloading :: String -> IO ()
downloading = (>>= writeFile "exemple.html") . openURL

-- Provided by a super programmer

main :: IO String
main = do
  manager <- newManager defaultManagerSettings
  request <- parseRequest "http://www.piaotian.net/html/7/7430/"
  response <- httpLbs request manager
  gbk <- ICU.open "gbk" Nothing
  -- return $ TI.showText $ ICU.toUnicode gbk $ BL.toStrict $ responseBody response
  return $ T.unpack $ ICU.toUnicode gbk $ BL.toStrict $ responseBody response
  -- putStrLn str
  -- let txt = ICU.toUnicode gbk $ BL.toStrict $ responseBody response
  -- return . T.decodeUtf8 . T.encodeUtf8 $ txt
  -- T.putStrLn txt

-- Also I need a similar structure to TexEditing, the reason I imported that module.






