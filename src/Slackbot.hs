{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Slackbot (
    MessageContext(..)
  , Slackbot(..)
  , runSlackbot
  , Outgoing(..)
  , Auth(..)
  , Self(..)
  , module Slackbot.Message
  ) where

import           Slackbot.Message

import           Control.Concurrent   (newMVar, putMVar, takeMVar)
import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad        (forever, void)
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.IO         as T
import           Network.URI
import           Network.WebSockets
import           Network.Wreq         (defaults, getWith, param, responseBody)
import           Wuss

runSlackbot :: String -> c -> Slackbot c -> IO ()
runSlackbot token context slackbot = do
  let opts = defaults & param "token" .~ [T.pack token]
  r <- getWith opts  "https://slack.com/api/rtm.start"
  case decode (r ^. responseBody) of
    Just auth@Auth{..}
      | Just uri <- parseURI authUrl
      , Just (URIAuth _ host _) <- uriAuthority uri
      ->
        do
          print auth
          runSecureClient host 443 (uriPath uri) (makeClientApp auth context slackbot)
    _ -> putStrLn "error"  

newtype Slackbot c
  = Slackbot {
    slackbot :: MessageContext -> c -> [MessagePart] -> ([Outgoing], c)
  }

data MessageContext =
  MessageContext {
    mcAuth    :: Auth
  , mcChannel :: Text
  }
  deriving (Show)

makeClientApp :: Auth -> c -> Slackbot c -> ClientApp ()
makeClientApp auth context bot conn = do
  putStrLn "Connected!"

  v <- newMVar context

  void . forever $ do
    message <- receiveData conn
    case decode message :: Maybe Incoming of
      Just im@IncomingMessage{..} -> do
        print im
        case parseMessage imText of
          Just msg -> do
            let mc = MessageContext auth imChannel
            ctx <- takeMVar v
            let (out, ctx') = slackbot bot mc ctx msg
            putMVar v ctx'
            mapM_ (sendTextData conn . encode) out
          Nothing ->
            putStrLn ">>> could not parse message text"
      Just IncomingHello -> putStrLn "HELLO"
      Just ReconnectUrl{..} -> return ()
      Just PresenceChange{..} -> return ()
      Nothing -> do
        putStrLn ">>> unknown message type:"
        putStr ">>> "
        T.putStrLn . T.decodeUtf8 . LBS.toStrict $ message

  sendClose conn ("Bye!" :: Text)

data Auth
  = Auth {
    authSelf     :: Self
  , authChannels :: [Channel]
  , authUsers    :: [User]
  , authUrl      :: String
  }
  deriving (Show)

instance FromJSON Auth where
  parseJSON =
    withObject "Auth" $ \v ->
      Auth
        <$> v .: "self"
        <*> v .: "channels"
        <*> v .: "users"
        <*> v .: "url"

data Self
  = Self {
    selfId   :: Text
  , selfName :: Text
  }
  deriving (Eq, Show)

instance FromJSON Self where
  parseJSON =
    withObject "Self" $ \v ->
      Self
        <$> v .: "id"
        <*> v .: "name"

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
  = IncomingHello
  | IncomingMessage {
    imTS      :: Text -- timestamp
  , imUser    :: Text
  , imText    :: Text
  , imChannel :: Text
  }
  | PresenceChange {
    pcPresence :: Text
  , pcUser     :: Text
  }
  | ReconnectUrl {
    ruUrl :: Text
  }
  deriving (Show)

instance FromJSON Incoming where
  parseJSON =
    withObject "Incoming" $ \v -> do
      type_ <- v .: "type"
      case type_ :: Text of
        "hello" -> return IncomingHello
        "message" ->
          IncomingMessage
            <$> v .: "ts"
            <*> v .: "user"
            <*> v .: "text"
            <*> v .: "channel"
        "presence_change" ->
          PresenceChange
            <$> v .: "presence"
            <*> v .: "user"
        "reconnect_url" ->
          ReconnectUrl <$> v .: "url"
        _ -> fail "unknown message type"

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
