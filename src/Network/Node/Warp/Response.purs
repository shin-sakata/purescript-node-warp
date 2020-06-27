module Network.Node.Warp.Response where

import Prelude

import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Network.HTTP.Types (Status(..))
import Network.Wai (Response(..))
import Network.Wai as Wai
import Node.Encoding (Encoding(..))
import Node.HTTP as HTTP
import Node.Stream as Stream

-- data Response
--     = ResponseFile H.Status H.ResponseHeaders FilePath
--     | ResponseString H.Status H.ResponseHeaders String
--     | ResponseStream H.Status H.ResponseHeaders (Readable ())
--     | ResponseRaw (Aff Buffer -> (Buffer -> Aff Unit) -> Aff Unit) Response

respond :: HTTP.Response -> Wai.Response -> Aff Wai.ResponseReceived
respond responseWriter response = case response of
    ResponseString (Status status) headers body -> do
        liftEffect $ HTTP.setStatusCode responseWriter status.code
        liftEffect $ HTTP.setStatusMessage responseWriter status.message
        liftEffect $ for_ headers (\(Tuple name val) -> HTTP.setHeader responseWriter name val)
        let writer = HTTP.responseAsStream responseWriter
        _ <- liftEffect $ Stream.writeString writer UTF8 body (Stream.end writer (pure unit))
        pure Wai.ResponseReceived
    -- TODO
    _ -> pure Wai.ResponseReceived
