{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import           Control.Concurrent (forkIO)
import           Control.Lens       ((&), (.~), (^.))
import           Control.Monad      (forever, unless, void)
import           Data.Aeson
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Network.URI
import           Network.WebSockets
import           Network.Wreq       (defaults, getWith, param, responseBody)
import           System.Environment (getArgs)
import           Text.Parsec
import           Text.Parsec.Text
import           Wuss

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

main :: IO ()
main = do
  [token] <- getArgs
  let opts = defaults & param "token" .~ [T.pack token]
  r <- getWith opts  "https://slack.com/api/rtm.start"
  case decode (r ^. responseBody) of
    Just auth@Auth{..}
      | Just uri <- parseURI authUrl
      , Just (URIAuth _ host _) <- uriAuthority uri
      -> bot auth host (uriPath uri)
    _ -> putStrLn "error"

bot :: Auth -> String -> String -> IO ()
bot auth host path = runSecureClient host 443 path (ws auth)

ws :: Auth -> ClientApp ()
ws auth conn = do
  putStrLn "Connected!"

  forever $ do
    message <- receiveData conn
    case decode message :: Maybe Incoming of
      Just im@IncomingMessage{..} -> do
        print im
        case parse p mempty imText of
          Right ast -> do
            let om = OutgoingMessage 1 "message" imChannel (T.pack . show $ ast)
            sendTextData conn (encode om)
          Left e ->
            putStrLn ">>> NO PARSE"
      _ ->
        putStrLn ">>> unknown incoming message format"

  sendClose conn ("Bye!" :: Text)

data Term
  = F Text [Term]
  | V Text
  deriving (Show)

term :: Parser Term
term = try fun <|> var
  where
    ident :: Parser Text
    ident = T.pack <$> many1 letter

    fun :: Parser Term
    fun = do
      f <- ident
      char '('
      ts <- term `sepBy` char ','
      char ')'
      return $ F f ts

    var :: Parser Term
    var = V <$> ident

p :: Parser Term
p = do
  char '<'
  char '@'
  many1 alphaNum
  char '>'
  many1 space
  term
