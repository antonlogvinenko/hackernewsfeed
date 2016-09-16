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
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Either.Utils (maybeToEither)
import Data.Bool (bool)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)

data Failure = ListGet |
               ListParse |
               StoryGet { story :: Int } |
               StoryParse { story :: Int } |
               NoPublishedDB |
               PublishedDBParse
             deriving Show

-- Identifiers
type StoryIdentifiers = [Int]

getTopIds :: ExceptT Failure IO StoryIdentifiers
getTopIds = do
  response <- liftIO $ get "https://hacker-news.firebaseio.com/v0/topstories.json"
  let code = response ^. responseStatus ^. statusCode
  if code == 200
    then maybeToEither ListParse $ decode $ response ^. responseBody
    else throwE ListGet


-- Story
data Story = Story {
  key :: Int,
  score :: Int,
  title :: String,
  url :: String
  } deriving Show

instance FromJSON Story where
  parseJSON (Object v) =
    Story <$> v .: "id"
    <*> v .: "score"
    <*> v .: "title"
    <*> v .: "url"
    
formatStory :: Story -> String
formatStory s = (title s) ++ " " ++ (url s)


storyUrl :: Int -> String
storyUrl key = "https://hacker-news.firebaseio.com/v0/item/" ++ (show key) ++ ".json"

getStory :: Int -> ExceptT Failure IO Story
getStory key = do
  response <- liftIO $ get $ storyUrl key
  let code = response ^. responseStatus ^. statusCode
  if code == 200
    then maybeToEither (StoryParse key) $ decode $ response ^. responseBody
    else throwE (StoryGet key)



filterOutPublished :: StoryIdentifiers -> ExceptT Failure IO StoryIdentifiers
filterOutPublished ids = do
  published <- liftIO $ decodeStrict <$> BS.readFile dbFileName
  maybeToEither PublishedDBParse $ fmap (ids \\) published


dbFileName = "./published.db"

checkPublishedDBExists :: String -> ExceptT Failure IO ()
checkPublishedDBExists dbName = do
  exists <- liftIO $ doesFileExist dbName
  bool (throwE NoPublishedDB) (return ()) exists

publishStory :: Story -> ExceptT Failure IO ()
publishStory s = liftIO $ return ()


rememberStory :: String -> Story -> ExceptT Failure IO ()
rememberStory dbFileName story = do
  publishedIds <- liftIO $ decodeStrict <$> BS.readFile dbFileName
  let newContents = take 1000 $ (key story) : maybe [] id publishedIds
  liftIO $ BSL.writeFile dbFileName $ encode newContents


getTopStory :: StoryIdentifiers -> ExceptT Failure IO Story
getTopStory ids = do
  filtered <- filterOutPublished ids
  topStories <- mapM getStory $ take 10 filtered
  return $ head $ sortOn (Down . score) $ topStories


publishNews :: ExceptT Failure IO Story
publishNews = do
  checkPublishedDBExists dbFileName
  topIds <- getTopIds
  topStory <- getTopStory topIds
  publishStory topStory
  rememberStory dbFileName topStory
  return topStory


main :: IO ()
main = do 
  print "Started..."
  forever $ do
    x <- runExceptT publishNews
    print x
    threadDelay $ 60 * 1000000

