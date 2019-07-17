{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Lib
import qualified System.Environment            as E

main :: IO ()
main = do
  port  <- E.getEnv "PORT"
  token <- E.getEnv "SLACK_OAUTH_TOKEN"
  Lib.startApp (read port) token
