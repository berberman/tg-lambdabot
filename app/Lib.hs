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
import Control.Monad (void, when)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Eval
import GHC.Generics (Generic)
import Lens.Micro
import Network.HTTP.Req
import Polysemy
import Polysemy.Async
import Polysemy.State
import qualified Text.Megaparsec as M
import Prelude hiding (log)

data Message = Message
  { _updateId :: Integer,
    _messageId :: ReplyId,
    _chatId :: ChatID,
    _isPM :: Bool,
    _text :: Text,
    _sender :: User,
    _parentMsgId :: Maybe ReplyId,
    _inlineQuery :: Maybe InlineQuery
  }
  deriving (Show, Eq)

data InlineQuery = InlineQuery
  { _inline_id :: InlineId,
    _inline_text :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON InlineQuery where
  parseJSON = withObject "InlineQuery" $ \v -> InlineQuery <$> v .: "id" <*> v .: "query"

data User = User
  { first_name :: Text,
    last_name :: Maybe Text,
    username :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON User

prettyShowUser :: User -> Text
prettyShowUser User {..} = first_name <> fromMaybe " " last_name <> maybe "" (\u -> "(@" <> u <> ")") username

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

type InlineId = String

newtype UpdateState = UpdateState Integer deriving (Eq, Show)

data TgBot m a where
  Poll :: UpdateState -> TgBot m [Message]
  Reply :: (ChatID, Text, ReplyId) -> TgBot m Bool
  AnswerInlineQuery :: (Text, InlineId) -> TgBot m Bool
  SendChatAction :: ChatID -> TgBot m Bool

data Eval m a where
  CallLambda :: Cmd -> Expr -> Eval m String
  CallEval :: Expr -> Eval m String

data Logger m a where
  Log :: Text -> Logger m ()

makeSem ''TgBot
makeSem ''Eval
makeSem ''Logger

updateState :: Members [State UpdateState, Logger] r => [Message] -> Sem r ()
updateState [] = return ()
updateState messages = log (T.pack $ show new) >> put new
  where
    new = UpdateState . (+ 1) $ maximum $ messages <&> _updateId

reqToIO :: forall a r. Member (Embed IO) r => Req a -> Sem r a
reqToIO req = embed @IO $ runReq defaultHttpConfig req

tgBotToIO :: Members [Logger, Embed IO] r => Sem (TgBot ': r) a -> Sem r a
tgBotToIO = interpret $ \case
  Poll (UpdateState state) -> do
    let request = req POST getUpdatesAPI (ReqBodyJson $ object ["offset" .= state, "allowed_updates" .= Array ["message", "inline_query"]]) jsonResponse mempty
    response <- reqToIO request
    return . parseMessages $ responseBody response
    where
      messageParser (Object v) = do
        _updateId <- v .: "update_id"
        msg <- v .: "message"
        _messageId <- msg .: "message_id"
        _sender <- msg .: "from"
        chat <- msg .: "chat"
        _chatId <- chat .: "id"
        chatType :: Text <- chat .: "type"
        _text <- msg .: "text"
        let _isPM = chatType == "private"
        parentMsg :: Maybe Object <- msg .:? "reply_to_message"
        _parentMsgId <- case parentMsg of
          Just p -> p .: "message_id"
          _ -> return Nothing
        _inlineQuery <- v .:? "inline_query"
        return Message {..}
      parseMessages :: MyResponse [Value] -> [Message]
      parseMessages (MyResponse _ a) = catMaybes $ a <&> parseMaybe messageParser
  Reply (chatId, text, replyId) -> do
    let obj = object ["chat_id" .= chatId, "text" .= text, "reply_to_message_id" .= replyId]
        request :: Req (JsonResponse (MyResponse Value))
        request = req POST sendMessageAPI (ReqBodyJson obj) jsonResponse mempty
    response <- reqToIO request
    return $ response & ok . responseBody
  AnswerInlineQuery (text, queryId) -> do
    let obj = object ["inline_query_id" .= queryId, "results" .= Array [result]]
        result = object ["type" .= String "article", "id" .= (queryId <> "r"), "title" .= String "Point free", "input_message_content" .= content]
        content = object ["message_text" .= text, "parse_mode" .= String "MarkdownV2"]
        request :: Req (JsonResponse (MyResponse Value))
        request = req POST answerInlineQueryAPI (ReqBodyJson obj) jsonResponse mempty
    response <- reqToIO request
    return $ response & ok . responseBody
  SendChatAction chatId -> do
    let obj = object ["chat_id" .= chatId, "action" .= String "typing"]
        request :: Req (JsonResponse (MyResponse Value))
        request = req POST sendChatActionAPI (ReqBodyJson obj) jsonResponse mempty
    response <- reqToIO request
    return $ response & ok . responseBody

evalToIO :: Member (Embed IO) r => Sem (Eval ': r) a -> Sem r a
evalToIO = interpret $ \case
  CallLambda cmd expr -> embed . runCommand $ "@" <> replace cmd <> " " <> expr
  CallEval expr -> embed $ runEval expr
  where
    replace = T.unpack . T.replace "_" "-" . T.pack

loggerToIO :: Member (Embed IO) r => Sem (Logger ': r) a -> Sem r a
loggerToIO = interpret $ \(Log s) -> embed $ do
  now <- getCurrentTime
  T.putStrLn $
    T.unwords
      [T.pack $ formatTime defaultTimeLocale "%c" now, s]

program :: Members '[TgBot, State UpdateState, Logger, Async, Eval] r => Sem r ()
program = do
  state <- get
  messages <- poll state
  updateState messages
  sequenceConcurrently $ filter (not . T.null . _text) messages <&> messageHandler
  program

runApplication :: IO ()
runApplication = runFinal . embedToFinal @IO . asyncToIOFinal . loggerToIO . evalToIO . tgBotToIO . evalState (UpdateState 233) $ program

messageIdToReply :: Message -> ReplyId
messageIdToReply Message {..} = fromMaybe _messageId _parentMsgId

messageHandler :: Members '[TgBot, Eval, Async, Logger] r => Message -> Sem r ()
messageHandler m@Message {..} = do
  let z = M.parse (M.try parseStart M.<|> M.try parseHelp M.<|> (parseCmd M.<?> "a legal command")) "Message" (T.unpack _text)
      replyF text = void $ reply (_chatId, text, messageIdToReply m)
  log $ "[Message] " <> prettyShowUser _sender <> ": " <> _text
  case z of
    Left e -> do
      log "[Info] No parse"
      when _isPM $
        replyF $ T.pack . M.errorBundlePretty $ e
    Right (cmd, arg) -> do
      log $ "[Info] /" <> T.pack cmd <> " " <> T.pack arg
      async (sendChatAction _chatId)
      case cmd of
        "help" -> replyF $ T.pack helpMessage
        "start" -> replyF "Hi!"
        "eval" -> do
          result <- callEval arg
          let r = if null result then "QAQ" else result
          replyF $ T.pack r
        _ -> do
          result <- callLambda cmd arg
          let r = if null result then "QAQ" else result
          replyF . T.pack . replace' $ r