{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import API
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Maybe (catMaybes)
import qualified Data.Scientific as Scientific
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lens.Micro
import Network.HTTP.Req
import Polysemy
import Polysemy.Async
import Polysemy.Internal (send)
import Polysemy.State

data Message = Message
  { _updateId :: Integer,
    _messageId :: Integer,
    _chatId :: Integer,
    _text :: Text
  }
  deriving (Show, Eq)

data MyResponse a = MyResponse
  { ok :: Bool,
    result :: a
  }
  deriving (Show, Generic)

instance (FromJSON a) => FromJSON (MyResponse a)

type ChatID = Integer

type ReplyId = Integer

newtype UpdateState = UpdateState Integer deriving (Eq, Show)

data TgBot m a where
  Poll :: UpdateState -> TgBot m [Message]
  Reply :: (ChatID, Text, ReplyId) -> TgBot m Bool

makeSem ''TgBot

initialState :: Member (State UpdateState) r => Sem r ()
initialState = put $ UpdateState 233

updateState :: Member (State UpdateState) r => [Message] -> Sem r ()
updateState [] = return ()
updateState messages = put $ UpdateState . (+ 2) $ maximum $ messages <&> _updateId

reqToIO :: forall a r. Member (Embed IO) r => Req a -> Sem r a
reqToIO req = embed (runReq defaultHttpConfig req :: IO a)

tgBotToIO :: Member (Embed IO) r => Sem (TgBot ': r) a -> Sem r a
tgBotToIO = interpret $ \case
  Poll (UpdateState state) -> do
    let request = req POST getUpdatesAPI (ReqBodyJson $ object ["offset" .= state, "allowed_updates" .= Array ["message"]]) jsonResponse mempty
    response <- reqToIO request
    return . parseMessages $ responseBody response
    where
      parseMessages :: MyResponse [Value] -> [Message]
      parseMessages (MyResponse _ a) = messages
        where
          lookupInt k obj = do
            m <- H.lookup k obj
            case m of
              (Number s) -> case Scientific.floatingOrInteger s of
                (Right x) -> return x
                _ -> Nothing
              _ -> Nothing
          parseSingle (Object update) = do
            (Object message) <- H.lookup "message" update
            (Object chat) <- H.lookup "chat" message
            updateId <- lookupInt "update_id" update
            messageId <- lookupInt "message_id" message
            chatId <- lookupInt "id" chat
            let (String text) = H.lookupDefault "" "text" message
            return $ Message updateId messageId chatId text
          messages = catMaybes $ a <&> parseSingle
  Reply (chatId, text, replyId) -> do
    let obj = object ["chat_id" .= chatId, "text" .= text, "reply_to_message_id" .= replyId]
        request :: Req (JsonResponse (MyResponse Value))
        request = req POST sendMessageAPI (ReqBodyJson obj) jsonResponse mempty
    response <- reqToIO request
    return $ response & ok . responseBody

program :: Members '[TgBot, State UpdateState, Async] r => Sem r ()
program = do
  state <- get
  messages <- poll state
  updateState messages
  sequenceConcurrently $ (filter ((/= T.empty) . _text) messages) <&> messageHandler
  program

runApplication :: IO ()
runApplication = runFinal . embedToFinal @IO . asyncToIOFinal . tgBotToIO . evalState (UpdateState 233) $ program

messageHandler :: Member TgBot r => Message -> Sem r ()
messageHandler Message {..} = do
  reply (_chatId, _text, _messageId)
  return ()
