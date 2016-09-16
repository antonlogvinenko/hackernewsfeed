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
import Network.Wreq.Lens
import qualified Data.Text as T

import qualified Data.ByteString.Char8 as S8

data Failure = ListGet |
               ListParse |
               StoryGet { story :: Int } |
               StoryParse { story :: Int } |
               NoPublishedDB |
               PublishedDBParse |
               TweetProblem |
               NoCreds
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
  if exists
    then return ()
    else throwE NoPublishedDB


rememberStory :: String -> Story -> ExceptT Failure IO ()
rememberStory dbFileName story = do
  publishedIds <- liftIO $ decodeStrict <$> BS.readFile dbFileName
  let newContents = take 1000 $ (key story) : maybe [] id publishedIds
  liftIO $ BSL.writeFile dbFileName $ encode newContents


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
  let cmd = "https://api.twitter.com/1.1/statuses/update.json"
      emptyBody = toJSON (Nothing :: Maybe String)
  response <- liftIO $ postWith
              ((defaults {Network.Wreq.Types.auth=Just authCreds}) & param "status".~ [T.pack text])
              cmd
              emptyBody
  let code = response ^. responseStatus ^. statusCode
  if code == 200
    then return ()
    else throwE TweetProblem
         

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
  oauth <- getOAuthCreds "./secret"
  tweet (formatStory topStory) oauth
  rememberStory dbFileName topStory
  return topStory


main :: IO ()
main = do 
  print "Started..."
  forever $ do
    x <- runExceptT publishNews
    print x
    threadDelay $ 60 * 1000000

