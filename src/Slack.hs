{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Slack
  ( channelTimes
  )
where

import qualified Time
import           Control.Concurrent.Async       ( mapConcurrently )
import qualified Data.Aeson.TH                 as JSONTH
import           Data.Function                  ( (&) )
import           Data.List                      ( sortOn )
import qualified Data.Set                      as Set
import qualified Network.HTTP.Simple           as HTTP

type MemberList = [String]

newtype TestResp = TestResp
  { ok :: Bool
  } deriving (Show)
$(JSONTH.deriveJSON JSONTH.defaultOptions ''TestResp)

newtype Channel = Channel
  { members :: MemberList
  } deriving (Show)
$(JSONTH.deriveJSON JSONTH.defaultOptions ''Channel)

newtype ChannelResp = ChannelResp
  { channel :: Channel
  } deriving (Show)
$(JSONTH.deriveJSON JSONTH.defaultOptions ''ChannelResp)

data User = User
  { deleted :: Bool
  , tz_label :: Maybe String
  , tz_offset :: Maybe Int
  } deriving (Show)
$(JSONTH.deriveJSON JSONTH.defaultOptions ''User)

newtype UserResp = UserResp
  { user :: User
  } deriving (Show)
$(JSONTH.deriveJSON JSONTH.defaultOptions ''UserResp)

data TimeZone = TimeZone
  { tzLabel :: String
  , tzOffset :: Int
  } deriving (Eq, Ord, Show)

channelTimes :: String -> String -> IO [String]
channelTimes token channel = do
  chanResp      <- channelInfoReq token channel
  userResponses <- getUsers token chanResp
  userResponses
    & fmap user
    & filter (not . deleted)
    & fmap userToTimezone
    & sortOn (\u -> (tzOffset u, tzLabel u))
    & Set.fromAscList
    & Set.toList
    & mapM timezoneToTime

slackAPI :: String
slackAPI = "https://slack.com/api/"

channelInfoReq :: String -> String -> IO ChannelResp
channelInfoReq token channel = do
  resp <- HTTP.httpJSON
    (HTTP.parseRequest_ $ concat
      [slackAPI, "channels.info", "?token=", token, "&channel=", channel]
    )
  return (HTTP.getResponseBody resp)

userInfoReq :: String -> String -> IO UserResp
userInfoReq token user = do
  resp <- HTTP.httpJSON
    ( HTTP.parseRequest_
    $ concat [slackAPI, "users.info", "?token=", token, "&user=", user]
    )
  return (HTTP.getResponseBody resp)

getUsers :: String -> ChannelResp -> IO [UserResp]
getUsers token ChannelResp { channel = Channel { members = members } } =
  mapConcurrently (userInfoReq token) members

userToTimezone :: User -> TimeZone
userToTimezone User { tz_label = Just label, tz_offset = Just offset } =
  TimeZone { tzLabel = label, tzOffset = offset }

timezoneToTime :: TimeZone -> IO String
timezoneToTime TimeZone { tzLabel = label, tzOffset = offset } = do
  timeString <- Time.getCurrentTimeWithOffset offset
  return $ concat [timeString, " : ", label]
