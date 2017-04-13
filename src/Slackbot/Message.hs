module Slackbot.Message where

import           Data.Text        (Text)
import qualified Data.Text        as T
import           Text.Parsec
import           Text.Parsec.Text

type Message = [MessagePart]

data MessagePart
  = Message {
    msgText :: Text
  }
  | UserRef {
    urId :: Text
  }
  | ChannelRef {
    crId   :: Text
  , crName :: Text
  }
  | UrlRef {
    crUrl :: Text
  }
  deriving (Eq, Show)

parseMessage :: Text -> Maybe [MessagePart]
parseMessage = either (const Nothing) Just . parse userMessage mempty

userMessage :: Parser [MessagePart]
userMessage =
    many1 $ msg <|> ref
  where
    msg = Message . T.pack <$> many1 (noneOf "<")

ref :: Parser MessagePart
ref = try userRef <|> try channelRef <|> urlRef

-- Example: <@U4WLZM16J>
userRef :: Parser MessagePart
userRef = do
  char '<'
  char '@'
  i <- T.pack <$> many1 alphaNum
  char '>'
  return $ UserRef $ i

-- Example: <#C4WLY8BT8|random>
channelRef :: Parser MessagePart
channelRef = do
  char '<'
  char '#'
  i <- T.pack <$> many1 alphaNum
  char '|'
  n <- T.pack <$> many1 alphaNum
  char '>'
  return $ ChannelRef i n

-- Example: <http://www.google.at>
urlRef :: Parser MessagePart
urlRef = do
  char '<'
  url <- T.pack <$> many1 (noneOf ">")
  char '>'
  return $ UrlRef url

addresses :: Message -> [Text] -> Bool
addresses m users = not . null . filter p $ m
  where
    p (UserRef u) | u `elem` users = True
                  | otherwise      = False
    p _                            = False
