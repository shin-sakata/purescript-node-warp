-- example

module Test.Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Network.HTTP.Types as H
import Network.Node.Warp as Warp
import Network.Wai (Request(..))
import Network.Wai as Wai

main :: Effect Unit
main = Warp.run 8080 routingApp

routingApp :: Wai.Application
routingApp (Request req) =
  case req.pathInfo of
    ["sleep"] -> sleepApp (Request req)
    _ -> rootApp (Request req)

rootApp :: Wai.Application
rootApp req respond = 
  respond $ Wai.responseString H.ok200 [Tuple H.hContentType "text/plain; charset=UTF8"] "Hello world!!"

sleepApp :: Wai.Application
sleepApp req respond = do
    delay $ Milliseconds 10000.0
    liftEffect $ log "sleeping...."
    respond $ Wai.responseString H.ok200 [Tuple H.hContentType "text/plain; charset=UTF8"] "Wake Up!!"