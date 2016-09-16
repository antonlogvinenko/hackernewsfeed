{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy (ByteString, readFile, writeFile)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Text.RawString.QQ
import Data.List (sortOn, (\\))
import Control.Lens
import Network.Wreq
import Data.String
import Data.Maybe (catMaybes)
import Data.Ord (Down(..))
import System.Directory


-- Identifiers
type StoryIdentifiers = [Int]

parseIdentifiers :: ByteString -> Maybe StoryIdentifiers
parseIdentifiers = decode


-- Story
data Story = Story {
  key :: Int,
  score :: Int,
  title :: String,
  url :: String
  } deriving Show

parseStory :: ByteString -> Maybe Story
parseStory = decode

sortStories :: [Story] -> [Story]
sortStories = sortOn (Down . score)

formatStory :: Story -> String
formatStory s = (title s) ++ " " ++ (url s)


getTopStories :: IO (Maybe StoryIdentifiers)
getTopStories = do
  top <- get "https://hacker-news.firebaseio.com/v0/topstories.json"
  return . parseIdentifiers $ top ^. responseBody

parseStories :: [Maybe ByteString] -> [Story]
parseStories = catMaybes . (map parseStory) . catMaybes

getStoryUrl :: Int -> String
getStoryUrl key = "https://hacker-news.firebaseio.com/v0/item/" ++ (show key) ++ ".json"

getStory :: Int -> IO (Maybe ByteString)
getStory key = do
  response <- get $ getStoryUrl key
  let code = response ^. responseStatus ^. statusCode
  return $ if code == 200
    then Just $ response ^. responseBody
    else Nothing


instance FromJSON Story where
  parseJSON (Object v) =
    Story <$> v .: "id"
    <*> v .: "score"
    <*> v .: "title"
    <*> v .: "url"


filterOutPublished :: StoryIdentifiers -> IO StoryIdentifiers
filterOutPublished ids = do
  published <- decodeStrict <$> BS.readFile dbFileName
  case published of
    Nothing -> return ids
    (Just publishedIds) -> return $ ids \\ publishedIds

--access twitter
--schedule
                     
dbFileName = "./published.db"

checkPublishedDBExists :: String -> IO ()
checkPublishedDBExists dbName = do
  exists <- doesFileExist dbName
  if exists
    then return ()
    else BSL.writeFile dbName ""

publishStory :: Story -> IO ()
publishStory s = print s


rememberStory :: String -> Story -> IO ()
rememberStory dbFileName story = do
  publishedIds <- decodeStrict <$> BS.readFile dbFileName
  let newContents = take 1000 $ (key story) : maybe [] id publishedIds
  BSL.writeFile dbFileName $ encode newContents
      

main :: IO ()
main = do
  checkPublishedDBExists dbFileName
  mbCurrentTopIds <- getTopStories
  case mbCurrentTopIds of
    Nothing -> print "Out of luck!"
    (Just currentTopIds) -> do
      topIds <- filterOutPublished currentTopIds
      topStoriesText <- mapM getStory $ take 10 topIds
      let topStory = head $ sortStories $ parseStories topStoriesText
      publishStory topStory
      rememberStory dbFileName topStory
  putStrLn "hello world"
