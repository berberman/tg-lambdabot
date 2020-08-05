{-# LANGUAGE OverloadedStrings #-}

module API where

import Network.HTTP.Req

baseAPI = https "api.telegram.org" /: "bot<token>"

getUpdatesAPI = baseAPI /: "getUpdates"

sendMessageAPI = baseAPI /: "sendMessage"