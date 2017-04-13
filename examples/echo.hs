{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import           Slackbot

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
    f MessageContext{..} context message =
        if message `addresses` [selfId . authSelf $ mcAuth]
          then ([yes], context)
          else ([no],  context)
      where
        yes = OutgoingMessage 1 "message" mcChannel "YES"
        no  = OutgoingMessage 1 "message" mcChannel "NO"
