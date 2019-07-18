{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Slack
  ( allUsersReq
  , channelSlackIDs
  )
where

import qualified Data.ByteString.Char8         as B8
import           Control.Concurrent             ( threadDelay )
import           Data.Aeson
import           Data.Foldable                  ( asum )
import           Data.Function                  ( (&) )
import qualified Network.HTTP.Simple           as HTTP

import qualified Db

type MemberList = [String]

newtype Channel = Channel
  { members :: MemberList
  } deriving (Show)

instance FromJSON Channel where
  parseJSON (Object v) = do
    channel <- v .: "channel"
    members <- channel .: "members"
    return Channel { .. }

data AllUsersResp = AllUsersResp
  { users :: [Db.SlackUser]
  , next_cursor :: Maybe String
  } deriving (Show)

instance FromJSON AllUsersResp where
  parseJSON (Object v) = do
    members     <- v .: "members"
    users       <- parseJSON (Array members)
    next_cursor <- asum
      [ do
        m <- v .: "response_metadata"
        m .:? "next_cursor"
      , return Nothing
      ]
    return AllUsersResp { .. }

channelSlackIDs :: String -> String -> IO [String]
channelSlackIDs token channel = do
  chanResp <- channelInfoReq token channel
  return $ members chanResp

slackAPI :: String
slackAPI = "https://slack.com/api/"

channelInfoReq :: String -> String -> IO Channel
channelInfoReq token channel = do
  resp <- HTTP.httpJSON
    (HTTP.parseRequest_ $ concat
      [slackAPI, "channels.info", "?token=", token, "&channel=", channel]
    )
  return (HTTP.getResponseBody resp)

allUsersReqURL :: String -> Maybe String -> [String]
allUsersReqURL token Nothing =
  [slackAPI, "users.list", "?token=", token, "&include_locale=true&limit=200"]
allUsersReqURL token (Just cursor) =
  allUsersReqURL token Nothing <> ["&cursor=", cursor]

allUsersReq :: String -> IO [Db.SlackUser]
allUsersReq token = do
  resp <- doRequestAllUsers $ concat $ allUsersReqURL token Nothing
  allUsersReq_ token resp

allUsersReq_ :: String -> AllUsersResp -> IO [Db.SlackUser]
allUsersReq_ token AllUsersResp { next_cursor = Nothing, users = users } =
  return users
allUsersReq_ token AllUsersResp { next_cursor = Just "", users = users } =
  return users
allUsersReq_ token AllUsersResp { next_cursor = Just cursor, users = users } =
  do
    resp      <- doRequestAllUsers $ concat $ allUsersReqURL token (Just cursor)
    nextUsers <- allUsersReq_ token resp
    return $ users <> nextUsers

doRequestAllUsers :: String -> IO AllUsersResp
doRequestAllUsers url = do
  resp <- HTTP.httpJSONEither $ HTTP.parseRequest_ url
  case HTTP.getResponseBody resp of
    Left err -> case HTTP.getResponseStatusCode resp of
      429 -> do
        let retryAfter = read
              (B8.unpack (head (HTTP.getResponseHeader "Retry-After" resp)))
        threadDelay $ retryAfter * 1000000
        doRequestAllUsers url
      status -> error $ "uncatchable error: " <> show (status, err)
    Right allUsersResp -> return allUsersResp
