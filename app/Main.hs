{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8         as B8
import qualified System.Environment            as E

import qualified Db
import qualified Lib

main :: IO ()
main = do
  (action : _) <- E.getArgs
  start action

start :: String -> IO ()

start "serve" = do
  print "starting server"
  port  <- E.getEnv "PORT"
  token <- E.getEnv "SLACK_OAUTH_TOKEN"
  pgURL <- B8.pack <$> E.getEnv "DATABASE_URL"
  Lib.startApp (read port) token pgURL

start "syncSlackUsers" = do
  print "syncing slack users"
  token        <- E.getEnv "SLACK_OAUTH_TOKEN"
  pgURL        <- E.getEnv "DATABASE_URL"
  affectedRows <- Lib.syncDb token $ B8.pack pgURL
  print $ "Affected Rows: " <> show affectedRows
