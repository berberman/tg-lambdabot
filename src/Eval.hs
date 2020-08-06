module Eval where

import System.Process
import System.Exit

buildEval x = shell $ "cd ../mueval && stack exec -- mueval -e \"" ++ x ++ "\" && cd ../tg-echobot"

buildCommand x = shell $ "lambdabot -e \"" ++ x ++ "\""

runShell :: CreateProcess -> IO String
runShell s = do
  (exit, out, err) <- readCreateProcessWithExitCode s ""
  case exit of
    (ExitFailure _) -> return err
    _ -> return out

runEval x = runShell $ buildEval x

runCommand x = runShell $ buildCommand x