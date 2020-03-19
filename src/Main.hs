{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import Data.Semigroup (Endo (..))
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Hash.MD5 as MD5
import Network.HTTP.Client (defaultRequest)
import Network.HTTP.Types (renderQuery, queryTextToQuery)
import Network.HTTP.Req
    ( runReq
    , req
    , defaultHttpConfig
    , GET (..)
    , NoReqBody (..)
    , https
    , bsResponse
    , responseBody
    , (=:) --query params
    , (/:)
    , Option (Option)
    , Url
    )

import FSMemoize (fsmemoize)

-- login page (new): https://leftypol.booru.org/index.php?page=login
-- posts page (old): https://lefty.booru.org/index.php?page=post&s=list&tags=all&pid=0

old_booru_base_url = https "leftypol.booru.org" /: "index.php"
old_booru_base_params
    = "page" =: ("post" :: String)
    <> "s" =: ("list" :: String)
    <> "tags" =: ("all" :: String)
--    <> "pid" =: 0

getRawPageBody_ :: Url scheme -> Option scheme -> IO ByteString
getRawPageBody_ url params = runReq defaultHttpConfig $ do
    r <- req
        GET
        url
        NoReqBody
        bsResponse
        params

    liftIO $ return $ responseBody r

getRawPageBody :: String -> Url scheme -> Option scheme -> IO ByteString
getRawPageBody datadir url params =
    fsmemoize
        datadir
        (uncurry hashUrl)
        (uncurry getRawPageBody_)
        (url, params);

renderParams :: Option scheme -> ByteString
renderParams (Option f _) = renderQuery True (queryTextToQuery params)
    where
        params = fst $ appEndo f ([], defaultRequest)

hashUrl :: Url scheme -> Option scheme -> String
hashUrl url params = BS.unpack $ MD5.hash $ BS.concat
    [ BS.pack (show url)
    , renderParams params
    ]

main :: IO ()
main = do
    args <- getArgs
    let datadir = head args
    
    bodybs <- getRawPageBody
        datadir
        old_booru_base_url
        (old_booru_base_params <> "pid" =: (0 :: Int))

    BS.putStrLn bodybs
    BS.putStrLn (MD5.hash bodybs)

{-
    r <- req
        GET
        (https "leftypol.booru.org" /: "index.php")
        NoReqBody
        bsResponse
        ("page" =: ("login" :: String) <> "code" =: ("00" :: String))
-}
