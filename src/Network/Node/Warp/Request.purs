module Network.Node.Warp.Request (abstraction) where

import Prelude
import Prim

import Data.Array (tail)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toMaybe)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (toArrayWithKey)
import Network.HTTP.Types as H
import Network.Wai as Wai
import Node.HTTP as HTTP
import Node.Stream (Readable)
import Node.URL (URL, parse)
import URI.Extra.QueryPairs as QE
import URI.Query as Q

abstraction :: HTTP.Request -> Wai.Request
abstraction req = Wai.Request {
     method: toMethod req
  ,  httpVersion: toHttpVersion req
  ,  rawPathInfo: toRawPathInfo req
  ,  rawQueryString: toRawQueryString req
  ,  requestHeaders: toRequestHeaders req
  ,  pathInfo: toPathInfo req
  ,  queryString: toQueryString req
  ,  body: toBody req
  }

--- Private

toMethod :: HTTP.Request -> H.Method
toMethod req = case H.parseMethod $ HTTP.requestMethod req of
    Right method -> method
    Left _ -> H.GET

toHttpVersion :: HTTP.Request -> H.HttpVersion
toHttpVersion req = case HTTP.httpVersion req of
    "0.9" -> H.http09
    "1.0" -> H.http10
    "1.1" -> H.http11
    "2.0" -> H.http20
    _ -> H.http10

toRawPathInfo :: HTTP.Request -> String
toRawPathInfo req = case toMaybe ((toUrl req).pathname) of
    Just rawPathInfo -> rawPathInfo
    Nothing -> ""

toRawQueryString :: HTTP.Request -> String
toRawQueryString req = case toMaybe ((toUrl req).search) of
    Just rawQueryString -> rawQueryString
    Nothing -> ""

toRequestHeaders :: HTTP.Request -> H.RequestHeaders
toRequestHeaders = HTTP.requestHeaders >>> toArrayWithKey Tuple

-- /hoge1/hoge2?query1=value1&query2=value2
-- >> ["hoge1", "hoge2"]
toPathInfo :: HTTP.Request -> Array String
toPathInfo =
    toUrl 
        >>> _.pathname
        >>> toMaybe
        >>> fromMaybe ""
        >>> split (Pattern "/")
        >>> tail
        >>> fromMaybe []

-- /hoge1/hoge2?query1=value1&query2=value2
-- >> [(Tuple "query1" (Just "value1")),(Tuple "query2" (Just "value2"))]
toQueryString :: HTTP.Request -> H.Query
toQueryString =
    toUrl
        >>> _.query -- query1=value1&query2=vallue2
        >>> toMaybe
        >>> fromMaybe ""
        >>> Q.fromString
        >>> QE.parse (QE.keyToString >>> Right) (QE.valueToString >>> Right)
        >>> case _ of
            Right (QE.QueryPairs array) -> array
            _ -> []

toBody :: HTTP.Request -> Readable ()
toBody = HTTP.requestAsStream

toUrl :: HTTP.Request -> URL
toUrl req = parse $ HTTP.requestURL req
