{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Lens.Micro
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Command = Command
  { _cmd :: String,
    _help :: String
  }

data Module = Module
  { _name :: String,
    _cmdList :: [Command]
  }

lambdaModules =
  [ Module "free" [Command "free" "free <ident>. Generate theorems for free"],
    Module
      "djinn"
      [ Command "djinn_add" $ unlines ["djinn_add <expr>.", "Define a new function type or type synonym"],
        Command "djinn_del" $ unlines ["djinn_del <ident>.", "Remove a symbol from the environment"],
        Command "djinn_env" $ unlines ["djinn_env.", "Show the current djinn environment"],
        Command "djinn_names" $ unlines ["djinn_names.", "Show the current djinn environment, compactly."],
        Command "djinn_clr" $ unlines ["djinn_clr.", "Reset the djinn environment"],
        Command "djinn" $ unlines ["djinn <type>.", "Generates Haskell code from a type."]
      ],
    Module
      "pl"
      [ Command "pl" "pl <expr>. Play with pointfree code.",
        Command "unpl" "unpl <expr>. Make code pointier."
      ],
    Module
      "do"
      [ Command "do" $ unlines ["do <expr>", "Translate Monad operators to do notation."],
        Command "undo" $ unlines ["undo <expr>", "Translate do notation to Monad operators."]
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
      "eval"
      [Command "eval" "eval <expr>. Run haskell expression."]
  ]

allCommands = (lambdaModules <&> _cmdList) ^. each

helpMessage = unlines $ allCommands & mapped %~ (\c -> ((<> ": ") . ("/" <>) $ (c & _cmd)) <> "   " <> (c & _help))

parseRaw :: String -> Parser (String, String)
parseRaw s = do
  (_ : xs) <- string s
  return (xs, "")

parseHelp = parseRaw "/help"

parseStart = parseRaw "/start"

parseCmd :: Parser (String, String)
parseCmd = do
  ('/' : cmd) <- choice $ allCommands <&> try . string . ('/' :) . _cmd
  space <- (try $ char (' ') *> return ()) <|> eof
  rest <- manyTill anySingle eof
  return (cmd, rest)

replace' ('n' : '-' : xs) = 'n' : '_' : replace' xs
replace' ('s' : '-' : xs) = 's' : '_' : replace' xs
replace' ('@' : xs) = replace' xs
replace' (x : xs) = x : replace' xs
replace' "" = ""
