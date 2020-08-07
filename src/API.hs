{-# LANGUAGE OverloadedStrings #-}

module API where

import Network.HTTP.Req

baseAPI = https "api.telegram.org" /: "bot<>"

getUpdatesAPI = baseAPI /: "getUpdates"

sendMessageAPI = baseAPI /: "sendMessage"

answerInlineQueryAPI = baseAPI /: "answerInlineQuery"