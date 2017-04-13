{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import           Slackbot

import qualified Data.Text          as T
import           System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (token : _) -> runSlackbot token () echo
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " <token>"

echo :: Slackbot ()
echo = Slackbot f
  where
    f :: MessageContext -> () -> [MessagePart] -> ([Outgoing], ())
    f MessageContext{..} context message = ([om], context)
      where
        om = OutgoingMessage 1 "message" mcChannel (T.pack . show $ message) 
