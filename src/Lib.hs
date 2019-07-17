{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
  ( startApp
  )
where

import           Control.Monad.IO.Class         ( liftIO )
import           Data.Aeson
import           Data.Aeson.TH
import           Data.List                      ( intercalate )
import           GHC.Generics                   ( Generic )
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import qualified Slack
import           Web.FormUrlEncoded             ( FromForm )

newtype SlackCmdResp = SlackCmdResp
  { text :: String } deriving (Eq, Show)

newtype CmdReq = CmdReq
  { channel_id :: String } deriving (Eq, Show, Generic)

instance FromForm CmdReq

$(deriveJSON defaultOptions ''SlackCmdResp)

type API
  = "channel-time" :> ReqBody '[FormUrlEncoded] CmdReq :> Post '[JSON] SlackCmdResp

startApp :: Int -> String -> IO ()
startApp port token = run port $ app token

app :: String -> Application
app token = serve api $ server token

api :: Proxy API
api = Proxy

server :: String -> Server API
server = slackCmdResp

slackCmdResp :: String -> CmdReq -> Handler SlackCmdResp
slackCmdResp token cmdReq = do
  resp <- liftIO $ Slack.channelTimes token (channel_id cmdReq)
  return $ SlackCmdResp $ intercalate "\n" resp
