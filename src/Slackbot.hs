{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Slackbot where

import           Control.Lens       ((&), (.~), (^.))
import           Control.Monad      (forever, void)
import           Data.Aeson
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Network.URI
import           Network.WebSockets
import           Network.Wreq       (defaults, getWith, param, responseBody)
import           Text.Parsec
import           Text.Parsec.Text
import           Wuss

runSlackbot :: String -> Slackbot () -> IO ()
runSlackbot token slackbot = do
  let opts = defaults & param "token" .~ [T.pack token]
  r <- getWith opts  "https://slack.com/api/rtm.start"
  case decode (r ^. responseBody) of
    Just auth@Auth{..}
      | Just uri <- parseURI authUrl
      , Just (URIAuth _ host _) <- uriAuthority uri
      -> runSecureClient host 443 (uriPath uri) (makeClientApp slackbot)
    _ -> putStrLn "error"  

newtype Slackbot c
  = Slackbot {
    slackbot :: MessageContext -> c -> [MessagePart] -> ([Outgoing], c)
  }

data MessageContext =
  MessageContext {
    mcChannel :: Text
  }
  deriving (Eq, Show)

makeClientApp :: Slackbot () -> ClientApp ()
makeClientApp bot conn = do
  putStrLn "Connected!"

  void . forever $ do
    message <- receiveData conn
    case decode message :: Maybe Incoming of
      Just im@IncomingMessage{..} -> do
        print im
        case parse userMessage mempty imText of
          Right msg -> do
            let mc = MessageContext imChannel
            let (out, _) = slackbot bot mc () msg
            mapM_ (sendTextData conn . encode) out
          Left _ ->
            putStrLn ">>> could not parse message text"
      _ ->
        putStrLn ">>> unknown incoming message format"

  sendClose conn ("Bye!" :: Text)

data Auth
  = Auth {
    authChannels :: [Channel]
  , authUsers    :: [User]
  , authUrl      :: String
  }
  deriving (Show)

instance FromJSON Auth where
  parseJSON =
    withObject "Auth" $ \v ->
      Auth
        <$> v .: "channels"
        <*> v .: "users"
        <*> v .: "url"

data Channel
  = Channel {
    chanId   :: Text
  , chanName :: Text
  }
  deriving (Show)

instance FromJSON Channel where
  parseJSON =
    withObject "Channel" $ \v ->
      Channel
        <$> v .: "id"
        <*> v .: "name"

data User
  = User {
    userId       :: Text
  , userName     :: Text
  , userRealName :: Text
  }
  deriving (Show)

instance FromJSON User where
  parseJSON =
    withObject "User" $ \v ->
      User
        <$> v .: "id"
        <*> v .: "name"
        <*> v .: "real_name"

data Incoming
  = IncomingMessage {
    imType    :: Text
  , imTS      :: Text -- timestamp
  , imUser    :: Text
  , imText    :: Text
  , imChannel :: Text
  }
  deriving (Show)

instance FromJSON Incoming where
  parseJSON =
    withObject "IncomingMessage" $ \v -> IncomingMessage
      <$> v .: "type"
      <*> v .: "ts"
      <*> v .: "user"
      <*> v .: "text"
      <*> v .: "channel"

data Outgoing
  = OutgoingMessage {
    omId      :: Int
  , omType    :: Text
  , omChannel :: Text
  , omText    :: Text
  }

instance ToJSON Outgoing where
  toJSON OutgoingMessage{..} =
    object
      [ "id"      .= omId
      , "type"    .= omType
      , "channel" .= omChannel
      , "text"    .= omText
      ]

-- * Message parsing

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
