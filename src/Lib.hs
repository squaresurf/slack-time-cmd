{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
  ( startApp
  , syncDb
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import qualified Data.Aeson.TH                 as JSONTH
import qualified Data.ByteString.Char8         as B8
import           Data.List                      ( intercalate )
import           GHC.Generics                   ( Generic )
import           GHC.Int                        ( Int64 )
import qualified Network.Wai.Handler.Warp      as Warp
import           Servant                        ( (:>)
                                                , Application
                                                , FormUrlEncoded
                                                , Handler
                                                , JSON
                                                , Post
                                                , Proxy(..)
                                                , ReqBody
                                                , Server
                                                , serve
                                                )
import           Web.FormUrlEncoded             ( FromForm )

import qualified Db
import qualified Slack
import qualified Time

newtype SlackCmdResp = SlackCmdResp
  { text :: String } deriving (Eq, Show)

newtype CmdReq = CmdReq
  { channel_id :: String } deriving (Eq, Show, Generic)

instance FromForm CmdReq

$(JSONTH.deriveJSON JSONTH.defaultOptions ''SlackCmdResp)

type API
  = "channel-time" :> ReqBody '[FormUrlEncoded] CmdReq :> Post '[JSON] SlackCmdResp

syncDb :: String -> B8.ByteString -> IO Int64
syncDb token dbURL = do
  users <- Slack.allUsersReq token
  Db.insertUsers dbURL users

startApp :: Int -> String -> B8.ByteString -> IO ()
startApp port token dbURL = Warp.run port $ app token dbURL

app :: String -> B8.ByteString -> Application
app token dbURL = serve api $ server token dbURL

api :: Proxy API
api = Proxy

server :: String -> B8.ByteString -> Server API
server = slackCmdResp

slackCmdResp :: String -> B8.ByteString -> CmdReq -> Handler SlackCmdResp
slackCmdResp token dbURL cmdReq = do
  slackIDs <- liftIO $ Slack.channelSlackIDs token (channel_id cmdReq)
  tzs      <- liftIO $ Db.selectTimeZones dbURL slackIDs
  msgs     <- liftIO $ mapM Time.dbTimeZoneToTime tzs
  return $ SlackCmdResp $ intercalate "\n" msgs
