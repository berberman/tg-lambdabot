
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib where

import           API
import           Control.Concurrent       (forkIO)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Aeson
import qualified Data.HashMap.Strict      as H
import           Data.Maybe
import qualified Data.Scientific          as Scientific
import           Data.Text                (Text)
import qualified Data.Text                as T
import           GHC.Generics             (Generic)
import           Lambdabot.Main
import           Lambdabot.Plugin.Haskell
import           Lens.Micro
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types
import           System.IO.Silently       (capture)
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Text

loadedModules = $(modules $ corePlugins ++ haskellPlugins)

-----------------------------------------------------------------------------

getUpdateRequest = parseRequest_ $ baseAPI ++ "getUpdates"

sendMessageRequest = parseRequest_ $ baseAPI ++ "sendMessage"

newtype UpdateState = UpdateState Integer

type Bot m = StateT UpdateState (ReaderT Manager m)

data Message = Message
    { _updateId  :: Integer
    , _messageId :: Integer
    , _chatId    :: Integer
    , _text      :: Text
    }
    deriving (Show, Eq)

data MyResponse a = MyResponse
    { ok     :: Bool
    , result :: a
    }
    deriving (Show, Generic)

instance (FromJSON a) => FromJSON (MyResponse a)

lookupInt k obj = do
  m <- H.lookup k obj
  case m of
    (Number s) -> case Scientific.floatingOrInteger s of
      (Right x) -> return x
      _         -> Nothing
    _ -> Nothing

updateMessages :: MonadIO m => Bot m [Message]
updateMessages = do
  manager <- ask
  (UpdateState state) <- get
  response <- liftIO $ httpLbs (getUpdateRequest {method = "POST", requestBody = RequestBodyLBS $ encode $ object ["offset" .= state], requestHeaders = [("Content-Type", "application/json")]}) manager
  let body = decode $ response & responseBody :: Maybe (MyResponse [Value])
      parseResult json = do
        (Object updates) <- json
        (Object message) <- H.lookup "message" updates
        (Object chat) <- H.lookup "chat" message
        updateId <- lookupInt "update_id" updates
        messageId <- lookupInt "message_id" message
        chatId <- lookupInt "id" chat
        (String text) <- H.lookup "text" message
        return $ Message updateId messageId chatId text
      messages = catMaybes $ case body of
        (Just x) -> x & result & mapped %~ parseResult . return
        Nothing  -> return Nothing
  liftIO $ putStrLn $ "Polled: " ++ show messages
  liftIO $ putStrLn $ "State: " ++ show state
  case length messages of
    0 -> return []
    _ -> do
      put . UpdateState . (+ 1) $ maximum $ messages <&> _updateId
      return messages

sendMessage :: MonadIO m => Integer -> Text -> Integer -> Bot m ()
sendMessage chatId text replyId = do
  manager <- ask
  let obj = object ["chat_id" .= chatId, "text" .= text, "reply_to_message_id" .= replyId]
  response <- liftIO $ httpLbs (sendMessageRequest {method = "POST", requestBody = RequestBodyLBS $ encode obj, requestHeaders = [("Content-Type", "application/json")]}) manager
  let body = decode $ response & responseBody :: Maybe (MyResponse Value)
  case body of
    Just (MyResponse status _) -> liftIO $ print status
    _                          -> return ()

runBot :: MonadIO m => Manager -> UpdateState -> Bot m a -> m a
runBot r s bot = runReaderT (evalStateT bot s) r

listenForever :: MonadIO m => Bot m ()
listenForever = do
  msg <- updateMessages
  manager <- ask
  state <- get
  forM_ msg (\m -> liftIO . forkIO . runBot manager state $ messageHandler m)
  listenForever

buildCmd x = onStartupCmds :=> [x]

runLambda c = capture $ void $ lambdabotMain loadedModules [c, consoleLogLevel :=> ERROR]

replace ('_' : xs) = '-' : replace xs
replace (x : xs)   = x : replace xs
replace ""         = ""

messageHandler :: MonadIO m => Message -> Bot m ()
messageHandler Message {..} = do
  let z = runP (try parseHelp <|> parseCmd) () "xxx" _text
  case z of
    Left e -> {- sendMessage _chatId (T.pack $ show e) _messageId -} return ()
    Right (cmd, arg) -> case cmd of
      "help" -> sendMessage _chatId helpMessage _messageId
      x -> do
        let builded = buildCmd $ '@' : (replace x) ++ " " ++ arg
        (result, _) <- liftIO $ runLambda builded
        sendMessage _chatId (T.pack result) _messageId


runApplication :: IO ()
runApplication = do
  manager <- newManager tlsManagerSettings
  runBot manager (UpdateState 233) listenForever

-----------------------------------------------------------------------------

data Command = Command
    { _cmd  :: Text
    , _help :: Text
    }

data Module = Module
    { _name    :: Text
    , _cmdList :: [Command]
    }

lambdaModules =
  [ Module "free" [Command "free" "free <ident>. Generate theorems for free"],
    Module
      "djinn"
      [ Command "djinn_add" $ T.unlines ["djinn_add <expr>.", "Define a new function type or type synonym"],
        Command "djinn_del" $ T.unlines ["djinn_del <ident>.", "Remove a symbol from the environment"],
        Command "djinn_env" $ T.unlines ["djinn_env.", "Show the current djinn environment"],
        Command "djinn_names" $ T.unlines ["djinn_names.", "Show the current djinn environment, compactly."],
        Command "djinn_clr" $ T.unlines ["djinn_clr.", "Reset the djinn environment"],
        Command "djinn" $ T.unlines ["djinn <type>.", "Generates Haskell code from a type.", "http://darcs.augustsson.net/Darcs/Djinn"]
      ],
    Module
      "check"
      [Command "check" "You have QuickCheck and 3 seconds. Prove something."],
    Module
      "pl"
      [ Command "pl" "pl <expr>. Play with pointfree code.",
        Command "unpl" "unpl <expr>. Make code pointier."
      ],
    Module
      "do"
      [ Command "do" $ T.unlines ["do <expr>", "Translate Monad operators to do notation."],
        Command "undo" $ T.unlines ["undo <expr>", "Translate do notation to Monad operators."]
      ],
    Module
      "mtl"
      [Command "unmtl" "unroll mtl monads"],
    Module
      "pretty"
      [Command "pretty" "pretty <expr>. Display haskell code in a pretty_printed manner"],
    Module
      "type"
      [ Command "type" "type <expr>. Return the type of a value",
        Command "kind" "kind <type>. Return the kind of a type"
      ],
    Module
      "instances"
      [ Command "instances_importing" "instances_importing [<module> [<module> [<module...]]] <typeclass>. Fetch the instances of a typeclass, importing specified modules first.",
        Command "instances" "instances <typeclass>. Fetch the instances of a typeclass."
      ],
    Module
      "hoogle"
      [Command "hoogle" "hoogle <expr>. Haskell API Search for either names, or types."],
    Module
      "src"
      [Command "src" "src <id>. Display the implementation of a standard function"]
  ]

allCommands = (lambdaModules <&> _cmdList) ^. each

helpMessage = T.unlines $ allCommands & mapped %~ (\c -> ((<> ": ") . ("/" <>) $ (c & _cmd)) <> "   " <> (c & _help))

parseHelp :: Parser (String, String)
parseHelp = do
  string "/help"
  return ("help", "")

parseCmd :: Parser (String, String)
parseCmd = do
  ('/': cmd) <- choice $ allCommands <&> try . string . ('/' :) . T.unpack . _cmd
  space <- (try $ char (' ') *> return ()) <|> eof
  rest <- manyTill anyChar eof
  return (cmd, rest)


-----------------------------------------------------------------------------

-- Libraries are old in the stackage of ghc 7.10...

infixl 1 <&>
(<&>) = flip fmap

(<>) = T.append

parseRequest_ s = case parseUrl s of
  Just x -> x