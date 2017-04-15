{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import           Slackbot

import           Data.Text          (Text)
import qualified Data.Text          as T
import           System.Environment (getArgs)
import           System.Random

main :: IO ()
main = do
  args <- getArgs
  case args of
    [token, path] -> hangbot token path
    [token]       -> hangbot token "/usr/share/dict/words" 

hangbot :: String -> String -> IO ()
hangbot token path = do
  words <- lines <$> readFile path
  (index, _) <- randomR (0, length words - 1) <$> getStdGen
  let word = words !! index
  runSlackbot token (mkHangman word) hangman

findText :: (Text -> Bool) -> [MessagePart] -> Maybe Text
findText p (x : xs)
  | Message t <- x
  , p t       = Just t
  | otherwise = findText p xs
findText _ [] = Nothing

findChar :: Message -> Maybe Char
findChar m = head . T.unpack <$> findText p m
  where
    p t = T.length t == 1

hangman :: Slackbot Hangman
hangman = Slackbot f
  where
    f MessageContext{..} game m
      | Just c <- findChar m =
              let ctx = guess c game
                  out m = OutgoingMessage 1 "message" mcChannel m
              in ([out (T.pack . partialSolution $ ctx)], ctx)
      | otherwise = ([], game)

data Hangman
  = Hangman {
    hmWord    :: String
  , hmGuesses :: [Char]
  }
  deriving Show

mkHangman :: String -> Hangman
mkHangman word =
  Hangman { hmWord = word
          , hmGuesses = mempty }

solved :: Hangman -> Bool
solved Hangman{..} = all (`elem` hmGuesses) hmWord

guess :: Char -> Hangman -> Hangman
guess c hm@Hangman{..} =
  hm { hmGuesses = c : hmGuesses }

partialSolution :: Hangman -> String
partialSolution Hangman{..} = map f hmWord
  where
    f c | c `elem` hmGuesses = c
        | otherwise          = '*'

play :: Hangman -> IO ()
play game@Hangman{..} =
  if solved game
    then putStrLn "Congratulations!"
    else do
      putStrLn $ partialSolution game
      line <- getLine
      case line of
        c : [] -> play (guess c game)
        _      -> do
          putStrLn "invalid input"
          play game
