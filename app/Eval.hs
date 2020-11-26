module Eval where

import System.Process

buildEval :: String -> CreateProcess
buildEval x = proc "mueval" ["-e " <> x]

buildCommand :: String -> CreateProcess
buildCommand x = proc "lambdabot" ["-e " <> x]

runShell :: CreateProcess -> IO String
runShell s = do
  (_, out, _) <- readCreateProcessWithExitCode s ""
  return out

runEval :: String -> IO String
runEval = runShell . buildEval

runCommand :: String -> IO String
runCommand = runShell . buildCommand