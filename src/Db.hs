{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}
module Db
  ( TimeZone
  , SlackUser
  , insertUsers
  , selectTimeZones
  )
where

import           Data.Aeson
import qualified Data.ByteString.Char8         as B8
import           Data.Function                  ( (&) )
import qualified Data.Set                      as Set
import qualified Database.PostgreSQL.Simple    as Pg
import           GHC.Generics                   ( Generic )
import           GHC.Int                        ( Int64 )

type TimeZone = (Int, String)

data SlackUser = SlackUser
  { slack_id :: String
  , deleted :: Bool
  , is_bot :: Bool
  , tz_offset :: Maybe Int
  , tz_label :: Maybe String
  } deriving (Show, Generic, Pg.ToRow)

instance FromJSON SlackUser where
  parseJSON (Object v) =
    Db.SlackUser
      <$> v
      .:  "id"
      <*> v
      .:  "deleted"
      <*> v
      .:  "is_bot"
      <*> v
      .:? "tz_offset"
      <*> v
      .:? "tz_label"

insertUsers :: B8.ByteString -> [SlackUser] -> IO Int64
insertUsers pgURL users = do
  conn <- Pg.connectPostgreSQL pgURL
  Pg.executeMany
    conn
    "INSERT INTO slack_users \
    \(slack_id, deleted, is_bot, tz_offset, tz_label) values (?, ?, ?, ?, ?) \
    \ON CONFLICT (slack_id) DO UPDATE SET \
    \(deleted, is_bot, tz_offset, tz_label) = (EXCLUDED.deleted, EXCLUDED.is_bot, EXCLUDED.tz_offset, EXCLUDED.tz_label)"
    users

selectTimeZones :: B8.ByteString -> [String] -> IO [TimeZone]
selectTimeZones pgURL slackIDs = do
  conn <- Pg.connectPostgreSQL pgURL
  res <- Pg.query
      conn
      "SELECT tz_offset, tz_label FROM slack_users \
    \WHERE slack_id in ? AND NOT deleted AND NOT is_bot \
    \ORDER BY tz_offset, tz_label"
    $ Pg.Only
    $ Pg.In slackIDs
  return $ res
    & Set.fromAscList
    & Set.toList
