module Network.Node.Warp where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Network.Node.Warp.Request (abstraction)
import Network.Node.Warp.Response (respond)
import Network.Wai as Wai
import Node.HTTP as HTTP

type Port = Int

implementation :: Wai.Application -> HTTP.Request -> HTTP.Response -> Aff Unit
implementation app req responseWriter = do
    _ <- app (abstraction req) (respond responseWriter)
    pure unit

run :: Port -> Wai.Application -> Effect Unit
run port app = do
    server <- HTTP.createServer (\req res -> launchAff_ $ implementation app req res)
    HTTP.listen server defaultOption { port = port } (pure unit)

defaultOption :: HTTP.ListenOptions
defaultOption = {backlog: Nothing, hostname: "localhost", port: 8080}
