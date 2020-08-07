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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import API
import Commands
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Maybe (catMaybes)
import qualified Data.Scientific as Scientific
import qualified Data.String.Utils as S
import Data.Text (Text)
import qualified Data.Text as T
import Debug.Trace
import Eval
import GHC.Generics (Generic)
import Lens.Micro
import Network.HTTP.Req
import Polysemy
import Polysemy.Async
import Polysemy.Internal (send)
import Polysemy.State
import qualified Text.Megaparsec as M

data Message = Message
  { _updateId :: Integer,
    _messageId :: Integer,
    _chatId :: Integer,
    _isPM :: Bool,
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

type Expr = String

type Cmd = String

newtype UpdateState = UpdateState Integer deriving (Eq, Show)

data TgBot m a where
  Poll :: UpdateState -> TgBot m [Message]
  Reply :: (ChatID, Text, ReplyId) -> TgBot m Bool

data Eval m a where
  CallLambda :: Cmd -> Expr -> Eval m String
  CallEval :: Expr -> Eval m String

makeSem ''TgBot
makeSem ''Eval

updateState :: Member (State UpdateState) r => [Message] -> Sem r ()
updateState [] = return ()
updateState messages = put $ UpdateState . (+ 1) $ maximum $ messages <&> _updateId

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
      parseMessages (MyResponse _ a) = catMaybes $ a <&> parseSingle
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
            chatType <- H.lookup "type" chat
            let (String text) = H.lookupDefault "" "text" message
            return $ Message updateId messageId chatId (chatType == "private") text
  Reply (chatId, text, replyId) -> do
    let obj = object ["chat_id" .= chatId, "text" .= text, "reply_to_message_id" .= replyId]
        request :: Req (JsonResponse (MyResponse Value))
        request = req POST sendMessageAPI (ReqBodyJson obj) jsonResponse mempty
    response <- reqToIO request
    return $ response & ok . responseBody

evalToIO :: Member (Embed IO) r => Sem (Eval ': r) a -> Sem r a
evalToIO = interpret $ \case
  CallLambda cmd expr -> embed . runCommand $ '@' : (S.replace "_" "-" cmd) ++ " " ++ expr
  CallEval expr -> embed $ runEval expr

program :: Members '[TgBot, State UpdateState, Async, Eval] r => Sem r ()
program = do
  state <- get
  messages <- poll state
  updateState messages
  sequenceConcurrently $ (filter ((/= T.empty) . _text) messages) <&> messageHandler
  program

runApplication :: IO ()
runApplication = runFinal . embedToFinal @IO . asyncToIOFinal . evalToIO . tgBotToIO . evalState (UpdateState 233) $ program

logResult :: Member TgBot r => Sem r Bool -> Sem r ()
logResult r = r >> return ()

messageHandler :: Members '[TgBot, Eval] r => Message -> Sem r ()
messageHandler Message {..} = do
  let z = M.parse (M.try parseHelp M.<|> (parseCmd M.<?> "a legal command")) "Message" (T.unpack _text)
  case (traceShow z z) of
    Left e ->
      if _isPM
        then logResult $ reply (_chatId, T.pack . M.errorBundlePretty $ e, _messageId)
        else return ()
    Right (cmd, arg) -> case cmd of
      "help" -> logResult $ reply (_chatId, T.pack helpMessage, _messageId)
      "eval" -> do
        result <- callEval arg
        let r = if result == [] then "Empty Body QAQ" else result
        logResult $ reply (_chatId, T.pack . replace' $ r, _messageId)
      _ -> do
        result <- callLambda cmd arg
        let r = if result == [] then "Empty Body QAQ" else result
        logResult $ reply (_chatId, T.pack . replace' $ r, _messageId)
  return ()
