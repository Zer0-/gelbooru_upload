{-# LANGUAGE OverloadedStrings, DataKinds #-}

import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)
import Control.Exception.Safe (handle)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client
    ( CookieJar
    , responseStatus
    )
import Network.HTTP.Types.Status (Status (statusCode))
import Network.HTTP.Req
    ( runReq
    , req
    , defaultHttpConfig
    , HttpConfig (..)
    , GET (..)
    , NoReqBody (..)
    , http
    , (/:)
    , port
    , bsResponse
    , responseBody
    , responseHeader
    , responseCookieJar
    , Option
    , Url
    , Scheme (..)
    , HttpException (..)
    )

type HttpResponseDat = (Maybe String, ByteString, CookieJar)
type HttpResponseWithMimeAndCookie = Either (Int, Maybe ByteString) HttpResponseDat

httpConfig :: HttpConfig
httpConfig = defaultHttpConfig { httpConfigBodyPreviewLength = 1024 * 5 }

local_server :: String
local_server = "127.0.0.1"

local_port :: Option scheme
local_port = port 8080

test_url :: Url 'Http
--test_url = http (Text.pack local_server) /: "shell.nix"
test_url = http (Text.pack local_server) /: "doesnt_exit.asdf"

httpGet :: Url scheme -> Option scheme -> IO HttpResponseWithMimeAndCookie
httpGet url params =
    runReq httpConfig $ do
        r <- req
            GET
            url
            NoReqBody
            bsResponse
            params

        liftIO $ return $ Right $
            ( responseHeader r "Content-Type" >>= return . BS.unpack
            , responseBody r
            , responseCookieJar r
            )

handler :: HttpException -> IO HttpResponseWithMimeAndCookie
handler
    ( VanillaHttpException
        ( HTTP.HttpExceptionRequest
            _ -- Request
            (HTTP.StatusCodeException resp bs)
        )
    ) = return $ Left (statusCode $ responseStatus resp, Just bs)
handler _ = return $ Left (0, Nothing)

main :: IO ()
main =
    -- httpGet test_url local_port
    handle handler (httpGet test_url local_port)
    >>= print
