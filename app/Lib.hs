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
{-# LANGUAGE ViewPatterns #-}

module Lib where

import API
import Commands
import Control.Applicative ((<|>))
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

data Message
  = TextMessage
      { _updateId :: Integer,
        _messageId :: ReplyId,
        _chatId :: ChatID,
        _isPM :: Bool,
        _text :: Text,
        _sender :: User,
        _parentMsgId :: Maybe ReplyId
      }
  | InlineMessage
      { _updateId :: Integer,
        _inlineQuery :: InlineQuery
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
  Reply :: ChatID -> Text -> ReplyId -> TgBot m Bool
  AnswerInlineQuery :: Text -> Text -> InlineId -> TgBot m Bool
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
      messageParser = withObject "Message" $ \v -> parseInline v <|> parseText v
      parseInline v = do
        _updateId <- v .: "update_id"
        _inlineQuery <- v .: "inline_query"
        return InlineMessage {..}
      parseText v = do
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
        return TextMessage {..}
      parseMessages :: MyResponse [Value] -> [Message]
      parseMessages (MyResponse _ a) = catMaybes $ a <&> parseMaybe messageParser
  (Reply chatId text replyId) -> do
    let obj = object ["chat_id" .= chatId, "text" .= text, "parse_mode" .= String "MarkdownV2", "reply_to_message_id" .= replyId]
        request :: Req (JsonResponse (MyResponse Value))
        request = req POST sendMessageAPI (ReqBodyJson obj) jsonResponse mempty
    response <- reqToIO request
    return $ response & ok . responseBody
  (AnswerInlineQuery title text queryId) -> do
    let obj = object ["inline_query_id" .= queryId, "results" .= Array [result]]
        result = object ["type" .= String "article", "id" .= (queryId <> "r"), "title" .= title, "input_message_content" .= content]
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
  sequenceConcurrently $ messages <&> messageHandler
  program

runApplication :: IO ()
runApplication = runFinal . embedToFinal @IO . asyncToIOFinal . loggerToIO . evalToIO . tgBotToIO . evalState (UpdateState 233) $ program

messageIdToReply :: Message -> ReplyId
messageIdToReply TextMessage {..} = fromMaybe _messageId _parentMsgId
messageIdToReply _ = undefined

wrapMarkdown :: String -> Text
wrapMarkdown text =
  T.unlines
    [ "```haskell",
      T.pack text,
      "```"
    ]

messageHandler :: Members '[TgBot, Eval, Async, Logger] r => Message -> Sem r ()
messageHandler InlineMessage {_inlineQuery = InlineQuery {_inline_text = T.unpack -> _inline_text, ..}} = do
  result <- callLambda "pl" _inline_text
  let text = T.concat ["Before:", wrapMarkdown _inline_text, "After:", wrapMarkdown result]
  void $ answerInlineQuery (T.pack result) text _inline_id
messageHandler m@TextMessage {..} = do
  let z = M.parse (M.try parseStart M.<|> M.try parseHelp M.<|> (parseCmd M.<?> "a legal command")) "Message" (T.unpack _text)
      prettySender = prettyShowUser _sender
      replyF text = do
        log $ "[Info] Reply\n" <> prettySender <> ": " <> text
        void $ reply _chatId (wrapMarkdown text) (messageIdToReply m)
  log $ "[Message] " <> prettySender <> (if _isPM then "[PM]" else "") <> ": " <> _text
  case z of
    Left e -> do
      log "[Info] No parse"
      when _isPM $
        replyF $ M.errorBundlePretty e
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
