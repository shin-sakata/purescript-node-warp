-- example

module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Network.Node.Warp as Warp
import Node.Encoding (Encoding(..))
import Node.HTTP as HTTP
import Node.Stream (end, onReadable, readString, writeString)

main :: Effect Unit
main = do
    server <- HTTP.createServer handler
    HTTP.listen server defaultOption (log "linsten")

defaultOption :: HTTP.ListenOptions
defaultOption = {backlog : Nothing, hostname: "localhost", port : 8080}

handler :: HTTP.Request -> HTTP.Response -> Effect Unit
handler req res = do
    let reqStream = HTTP.requestAsStream req
    onReadable reqStream do
      body <- readString reqStream Nothing UTF8
      log $ show $ body
    -- log $ show $ Warp.toUrl req
    log $ show $ Warp.toQueryString req
    HTTP.setStatusCode res 200
    HTTP.setHeader res "Content-Type" "text/html; charset=utf8"
    let stream = HTTP.responseAsStream res
    _ <- writeString stream UTF8 "hogeee" (pure unit)
    end stream (pure unit)
