module Anek (printRandomAnek) where

import System.Random

import Network.HTTP.Simple
import Text.HTML.TagSoup

import qualified Data.ByteString.Char8 as B8

printRandomAnek :: IO ()
printRandomAnek = getRandomAnek >>= B8.putStrLn

getRandomAnek :: IO B8.ByteString
getRandomAnek = getRandomId >>= getAnek where
  getRandomId = getStdRandom (randomR (1, 1142))

getAnek :: Int -> IO B8.ByteString
getAnek = fmap parseAnekBody . getAnekBody

getAnekBody :: Int -> IO B8.ByteString
getAnekBody = fmap getResponseBody . httpBS . request where
  request a = parseRequest_ $ "https://baneks.ru/" <> show a

parseAnekBody :: B8.ByteString -> B8.ByteString
parseAnekBody = attrContent . description . parseTags where
  description = head . dropWhile (~/= "<meta name=description>")
  attrContent = fromAttrib $ B8.pack "content"
