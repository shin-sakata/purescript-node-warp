module Network.Warp where

import Prelude
import Prim

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Tuple (Tuple(..))
import Foreign.Object (toArrayWithKey)
import Network.HTTP.Types as H
import Node.HTTP as HTTP
import Node.URL (URL, parse)

{-|
example
Request {
    requestMethod = "GET",
    httpVersion = HTTP/1.1, 
    rawPathInfo = "/one/two", 
    rawQueryString = "?query=value&query2=value2", 
    requestHeaders = [
        ("Host","localhost:8081"),
        ("Connection","keep-alive"),
        ("Cache-Control","max-age=0"),
        ("Upgrade-Insecure-Requests","1"),
        ("User-Agent","Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.116 Safari/537.36"),
        ("Accept","text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9"),
        ("Sec-Fetch-Site","none")
        ,("Sec-Fetch-Mode","navigate"),
        ("Sec-Fetch-User","?1"),
        ("Sec-Fetch-Dest","document"),
        ("Accept-Encoding","gzip, deflate, br"),
        ("Accept-Language","ja,en-US;q=0.9,en;q=0.8"),
    ],
    remoteHost = 127.0.0.1:49714,
    pathInfo = ["one","two"],
    queryString = [("query",Just "value"),("query2",Just "value2")],
    requestBody = <IO ByteString>,
    vault = <Vault>,
    requestBodyLength = KnownLength 0,
    requestHeaderHost = Just "localhost:8081",
    requestHeaderRange = Nothing
}
-}

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

-- Private

toUrl :: HTTP.Request -> URL
toUrl req = parse $ HTTP.requestURL req