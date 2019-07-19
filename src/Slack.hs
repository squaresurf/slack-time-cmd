{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Slack
  ( allUsersReq
  , conversationMembersReq
  )
where

import qualified Data.ByteString.Char8         as B8
import           Control.Concurrent             ( threadDelay )
import           Data.Aeson
import           Data.Aeson.Types               ( Parser )
import           Data.Foldable                  ( asum )
import           Data.Function                  ( (&) )
import qualified Network.HTTP.Simple           as HTTP

import qualified Db

newtype MemberList =
  MemberList (Maybe String, [String])
  deriving (Show)

instance FromJSON MemberList where
  parseJSON (Object v) = do
    members    <- v .: "members"
    nextCursor <- nextCursor (Object v)
    return $ MemberList (nextCursor, members)

data AllUsersResp = AllUsersResp
  { users :: [Db.SlackUser]
  , next_cursor :: Maybe String
  } deriving (Show)

instance FromJSON AllUsersResp where
  parseJSON (Object v) = do
    members     <- v .: "members"
    users       <- parseJSON (Array members)
    next_cursor <- nextCursor (Object v)
    return AllUsersResp { .. }

nextCursor :: Value -> Parser (Maybe String)
nextCursor (Object v) = asum
  [ do
    m <- v .: "response_metadata"
    c <- m .: "next_cursor"
    if c == "" then fail "empty string" else return (Just c)
  , return Nothing
  ]

slackAPI :: String
slackAPI = "https://slack.com/api/"

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

conversationMembersReqURL :: String -> String -> Maybe String -> [String]
conversationMembersReqURL token channel Nothing =
  [ slackAPI
  , "conversations.members"
  , "?limit=200&token="
  , token
  , "&channel="
  , channel
  ]
conversationMembersReqURL token channel (Just cursor) =
  conversationMembersReqURL token channel Nothing <> ["&cursor=", cursor]

conversationMembersReq :: String -> String -> IO [String]
conversationMembersReq token channel = do
  resp <- doRequestConversationMembers $ concat $ conversationMembersReqURL
    token
    channel
    Nothing
  conversationMembersReq_ token channel resp

conversationMembersReq_ :: String -> String -> MemberList -> IO [String]
conversationMembersReq_ token channel (MemberList (Nothing, members)) =
  return members
conversationMembersReq_ token channel (MemberList (Just cursor, members)) = do
  resp <- doRequestConversationMembers $ concat $ conversationMembersReqURL
    token
    channel
    (Just cursor)
  nextMembers <- conversationMembersReq_ token channel resp
  return $ members <> nextMembers

doRequestConversationMembers :: String -> IO MemberList
doRequestConversationMembers url = do
  resp <- HTTP.httpJSONEither $ HTTP.parseRequest_ url
  case HTTP.getResponseBody resp of
    Left err -> case HTTP.getResponseStatusCode resp of
      429 -> do
        let retryAfter = read
              (B8.unpack (head (HTTP.getResponseHeader "Retry-After" resp)))
        threadDelay $ retryAfter * 1000000
        doRequestConversationMembers url
      status -> error $ "uncatchable error: " <> show (status, err)
    Right conversationMembersResp -> return conversationMembersResp
