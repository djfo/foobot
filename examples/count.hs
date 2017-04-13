{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import           Slackbot

import qualified Data.Text          as T
import           System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  args <- getArgs
  case args of
    token : _ -> runSlackbot token 0 count
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " <token>"

count :: Slackbot Int
count = Slackbot f
  where
    f :: MessageContext -> Int -> [MessagePart] -> ([Outgoing], Int)
    f MessageContext{..} n message = ([om], n')
      where
        om = OutgoingMessage 1 "message" mcChannel (T.pack . show $ n')
        n' = n + 1
