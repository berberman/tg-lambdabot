{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (SomeException, catch)
import Lib

main :: IO ()
main = runApplication `catch` (\(e :: SomeException) -> print e >> runApplication)
