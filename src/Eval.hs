module Eval where

import           Mueval.ArgsParse
import           Mueval.Context
import           Mueval.Parallel
import           System.IO.Silently (capture)

defaultOptions :: Options
defaultOptions = Options { expression = ""
                           , modules = Just defaultModules
                           , timeLimit = 5
                           , user = ""
                           , loadFile = ""
                           , printType = True
                           , typeOnly = False
                           , extensions = True
                           , namedExtensions = []
                           , noImports = False
                           , rLimits = False
                           , packageTrust = False
                           , trustedPackages = defaultPackages
                           , help = False }

mueval e = fmap fst . capture . forkedMain $! defaultOptions{expression = e}