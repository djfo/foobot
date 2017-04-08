{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import           Slackbot

import qualified Data.Text          as T
import           System.Environment (getArgs)

main :: IO ()
main = do
  [token] <- getArgs
  runSlackbot token echo

echo :: Slackbot ()
echo = Slackbot f
  where
    f :: MessageContext -> () -> [MessagePart] -> ([Outgoing], ())
    f MessageContext{..} context message = ([om], context)
      where
        om = OutgoingMessage 1 "message" mcChannel (T.pack . show $ message) 
