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
import Commands
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Maybe (catMaybes)
import qualified Data.Scientific as Scientific
import qualified Data.String.Utils as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Void (Void)
import Debug.Trace
import GHC.Generics (Generic)
import Eval
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
    _text :: Text,
    _inline :: Maybe InlineQuery
  }
  deriving (Show, Eq)

data MyResponse a = MyResponse
  { ok :: Bool,
    result :: a
  }
  deriving (Show, Generic)

data InlineQuery = InlineQuery
  { _id :: InlineId,
    _query :: Text
  }
  deriving (Show, Eq)

instance (FromJSON a) => FromJSON (MyResponse a)

type ChatID = Integer

type InlineId = Text

type ReplyId = Integer

type Expr = String

type Cmd = String

newtype UpdateState = UpdateState Integer deriving (Eq, Show)

data TgBot m a where
  Poll :: UpdateState -> TgBot m [Message]
  Reply :: (ChatID, Text, ReplyId) -> TgBot m Bool
  AnswerInline :: (InlineId, Text) -> TgBot m Bool

data Eval m a where
  CallLambda :: Cmd -> Expr -> Eval m String
  CallEval :: Expr -> Eval m String

data WhatTheCommit m a where
  GenCommitMessage :: WhatTheCommit m Text

makeSem ''TgBot
makeSem ''Eval
makeSem ''WhatTheCommit

updateState :: Member (State UpdateState) r => [Message] -> Sem r ()
updateState [] = return ()
updateState messages = put $ UpdateState . (+ 1) $ maximum $ messages <&> _updateId

reqToIO :: forall a r. Member (Embed IO) r => Req a -> Sem r a
reqToIO req = embed (runReq defaultHttpConfig req :: IO a)

tgBotToIO :: Member (Embed IO) r => Sem (TgBot ': r) a -> Sem r a
tgBotToIO = interpret $ \case
  Poll (UpdateState state) -> do
    let request = req POST getUpdatesAPI (ReqBodyJson $ object ["offset" .= state, "allowed_updates" .= Array ["message","inline_query"]]) jsonResponse mempty
    response <- reqToIO request
    let m =parseMessages $ responseBody response
    return $ traceShow m m
    where
      parseMessages :: MyResponse [Value] -> [Message]
      parseMessages (MyResponse False _) = []
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
            return $ Message updateId messageId chatId text (parseInline update)
          messages = catMaybes $ a <&> parseSingle
          parseInline update = do
            (Object inline) <- H.lookup "inline_query" update
            (String id) <- H.lookup "id" inline
            (String query) <- H.lookup "query" update
            return $ InlineQuery id query
  Reply (chatId, text, replyId) -> do
    let obj = object ["chat_id" .= chatId, "text" .= text, "reply_to_message_id" .= replyId]
        request :: Req (JsonResponse (MyResponse Value))
        request = req POST sendMessageAPI (ReqBodyJson obj) jsonResponse mempty
    response <- reqToIO request
    return $ response & ok . responseBody
  AnswerInline (inlineId, text) -> do
    let obj = object ["inline_query_id" .= inlineId, "results" .= Array [object ["message_text" .= text]]]
        request :: Req (JsonResponse (MyResponse Void))
        request = req POST answerInlineQueryAPI (ReqBodyJson obj) jsonResponse mempty
    response <- reqToIO request
    return $ response & ok . responseBody

evalToIO :: Member (Embed IO) r => Sem (Eval ': r) a -> Sem r a
evalToIO = interpret $ \case
  CallLambda cmd expr -> embed . runCommand $ '@' : (S.replace "_" "-" cmd) ++ " " ++ expr
  CallEval expr -> embed $ runEval expr

whatTheCommitToIO :: Member (Embed IO) r => Sem (WhatTheCommit ': r) a -> Sem r a
whatTheCommitToIO = interpret $ \GenCommitMessage -> do
  let request :: Req BsResponse
      request = req GET (http "whatthecommit.com" /: "index.txt") NoReqBody bsResponse mempty
  response <- reqToIO request
  return . decodeUtf8 $ response & responseBody

program :: Members '[TgBot, State UpdateState, Async, Eval, WhatTheCommit] r => Sem r ()
program = do
  state <- get
  messages <- poll state
  updateState messages
  sequenceConcurrently $ (filter ((/= T.empty) . _text) messages) <&> messageHandler
  program

runApplication :: IO ()
runApplication = runFinal . embedToFinal @IO . asyncToIOFinal . whatTheCommitToIO . evalToIO . tgBotToIO . evalState (UpdateState 233) $ program

logResult :: Member TgBot r => Sem r Bool -> Sem r ()
logResult r = r >> return ()

messageHandler :: Members '[TgBot, Eval, WhatTheCommit] r => Message -> Sem r ()
messageHandler Message {..} = do
  case _inline of
    (Just InlineQuery {..}) -> do
      commit <- (traceShow "gen" genCommitMessage)
      logResult $ answerInline (_id, commit)
    _ -> do
      let z = M.parse (M.try parseHelp M.<|> parseCmd) "Command" (T.unpack _text)
      case (traceShow z z) of
        Left e -> return ()
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
