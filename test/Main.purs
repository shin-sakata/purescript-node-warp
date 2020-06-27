-- example

module Test.Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Network.HTTP.Types as H
import Network.Node.Warp as Warp
import Network.Wai (Request(..))
import Network.Wai as Wai

main :: Effect Unit
main = Warp.run 8080 routingApp

routingApp :: Wai.Application
routingApp (Request req) =
  case req.pathInfo of
    [""] -> rootApp (Request req)
    ["hello"] -> helloApp (Request req)
    _ -> notFoundApp (Request req)

rootApp :: Wai.Application
rootApp req respond = 
  respond $ Wai.responseString H.ok200 [Tuple H.hContentType "text/plain; charset=UTF8"] "Hello world!!"

helloApp :: Wai.Application
helloApp req respond =
  respond $ Wai.responseString H.ok200 [Tuple H.hContentType "text/plain; charset=UTF8"] "Hello!!"

notFoundApp :: Wai.Application
notFoundApp req respond =
  respond $ Wai.responseString H.notFound404 [Tuple H.hContentType "text/plain; charset=UTF8"] "NOT FOUND"