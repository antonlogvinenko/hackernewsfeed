{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Data.Aeson
import Data.ByteString.Lazy (ByteString, readFile, writeFile)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.List (sortOn, (\\))
import Control.Lens
import Network.Wreq (get, getWith, postWith, defaults, responseBody, statusCode, responseStatus)
import Data.Maybe (catMaybes)
import Data.Ord (Down(..))
import System.Directory
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Either.Utils (maybeToEither)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Network.Wreq.Types (auth, Auth(OAuth1))
import Network.Wreq.Lens (param)
import qualified Data.Text as T

import qualified Data.ByteString.Char8 as S8


-- Data types
type StoryIdentifiers = [Int]

data Story = Story {
  key :: Int,
  score :: Int,
  title :: String,
  url :: String
  }

instance Show Story where
  show s = (title s) ++ " " ++ (url s)

instance FromJSON Story where
  parseJSON (Object v) =
    Story <$> v .: "id"
    <*> v .: "score"
    <*> v .: "title"
    <*> v .: "url"

data Failure = ListGet |
               ListParse |
               StoryGet { story :: Int } |
               StoryParse { story :: Int } |
               NoPublishedDB |
               PublishedDBParse |
               TweetProblem |
               NoCreds
             deriving Show


-- HN API
getTopIds :: ExceptT Failure IO StoryIdentifiers
getTopIds = do
  response <- liftIO $ get "https://hacker-news.firebaseio.com/v0/topstories.json"
  let code = response ^. responseStatus ^. statusCode
  if code == 200
    then maybeToEither ListParse $ decode $ response ^. responseBody
    else throwE ListGet

getStory :: Int -> ExceptT Failure IO Story
getStory key = do
  response <- liftIO $ get $ "https://hacker-news.firebaseio.com/v0/item/" ++ (show key) ++ ".json"
  let code = response ^. responseStatus ^. statusCode
  if code == 200
    then maybeToEither (StoryParse key) $ decode $ response ^. responseBody
    else throwE (StoryGet key)

getTopStory :: StoryIdentifiers -> ExceptT Failure IO Story
getTopStory ids = do
  filtered <- filterOutPublished ids
  topStories <- mapM getStory $ take 10 filtered
  return $ head $ sortOn (Down . score) $ topStories


-- Twitter API
secretPath = "./secret"
emptyBody = toJSON (Nothing :: Maybe String)

getOAuthCreds :: FilePath -> ExceptT Failure IO Auth
getOAuthCreds path = do
  c <- liftIO $ decodeStrict <$> BS.readFile path
  let cred c n = S8.pack $ c !! n
  case (c :: Maybe [String]) of
    Nothing -> throwE NoCreds
    (Just c) -> return $ OAuth1 (cred c 0) (cred c 1) (cred c 2) (cred c 3)

tweet :: String -> Auth -> ExceptT Failure IO ()
tweet text authCreds = do
  liftIO $ print text
  response <- liftIO $ postWith
              (defaults {auth = Just authCreds} & param "status".~ [T.pack text])
              "https://api.twitter.com/1.1/statuses/update.json"
              emptyBody
  let code = response ^. responseStatus ^. statusCode
  if code == 200
    then return ()
    else throwE TweetProblem


-- Published identifiers storage
dbFileName = "./published.db"

checkPublishedDBExists :: String -> ExceptT Failure IO ()
checkPublishedDBExists dbName = do
  exists <- liftIO $ doesFileExist dbName
  if exists
    then return ()
    else throwE NoPublishedDB

filterOutPublished :: StoryIdentifiers -> ExceptT Failure IO StoryIdentifiers
filterOutPublished ids = do
  published <- liftIO $ decodeStrict <$> BS.readFile dbFileName
  maybeToEither PublishedDBParse $ fmap (ids \\) published

rememberStory :: String -> Story -> ExceptT Failure IO ()
rememberStory dbFileName story = do
  publishedIds <- liftIO $ decodeStrict <$> BS.readFile dbFileName
  let newContents = take 1000 $ (key story) : maybe [] id publishedIds
  liftIO $ BSL.writeFile dbFileName $ encode newContents


-- Main code
publishNews :: ExceptT Failure IO Story
publishNews = do
  checkPublishedDBExists dbFileName
  topIds <- getTopIds
  topStory <- getTopStory topIds
  oauth <- getOAuthCreds secretPath
  tweet (show topStory) oauth
  rememberStory dbFileName topStory
  return topStory

main :: IO ()
main = do 
  print "Started..."
  forever $ do
    x <- runExceptT publishNews
    print x
    threadDelay $ 60 * 1000000
