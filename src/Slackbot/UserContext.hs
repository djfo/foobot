module Slackbot.UserContext where

import           Slackbot

import           Data.Default
import           Data.Map     (Map)
import qualified Data.Map     as M
import           Data.Text    (Text)

userContextBot :: Default c => Slackbot c -> Slackbot (Map Text c)
userContextBot (Slackbot f) = Slackbot g
  where
    g msgctx@(MessageContext _ _ user) ctx msg =
      let userctx = maybe def id (M.lookup user ctx)
          (out, userctx') = f msgctx userctx msg
      in
      (out, M.insert user userctx' ctx)
