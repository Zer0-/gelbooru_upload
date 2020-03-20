{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Main where

import System.Environment (getArgs)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64.URL as BSURL
import Data.Semigroup (Endo (..))
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Hash.MD5 as MD5
import Network.HTTP.Client (defaultRequest, CookieJar)
import Network.HTTP.Types (renderQuery, queryTextToQuery)
import Network.HTTP.Req
    ( runReq
    , req
    , defaultHttpConfig
    , GET (..)
    , POST (..)
    , NoReqBody (..)
    , ReqBodyUrlEnc (..)
    , FormUrlEncodedParam
    , https
    , bsResponse
    , ignoreResponse
    , responseBody
    , responseCookieJar
    , (=:) --query params
    , (/:)
    , Option (Option)
    , Url
    , Scheme (..)
    )

import Pageparsers (imageLinks, imagePageFilenameTags)
import FSMemoize (fsmemoize)

-- login page (new): https://leftypol.booru.org/index.php?page=login
-- posts page (old): https://lefty.booru.org/index.php?page=post&s=list&tags=all&pid=0

old_booru_base_url :: Url 'Https
old_booru_base_url = https "lefty.booru.org" /: "index.php"

new_booru_base_url :: Url 'Https
new_booru_base_url = https "leftypol.booru.org" /: "index.php"

old_booru_base_params :: Option 'Https
old_booru_base_params
    =  "page" =: ("post" :: String)
    <> "s" =: ("list" :: String)
    <> "tags" =: ("all" :: String)

new_booru_login_params :: Option 'Https
new_booru_login_params
    =  "page" =: ("login" :: String)
    <> "code" =: ("00" :: String)

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
hashUrl url params = BS.unpack $ BSURL.encode $ MD5.hash $ BS.concat
    [ BS.pack (show url)
    , renderParams params
    ]


fetchOldBooruPage :: String -> Int -> IO [ Option 'Https ]
fetchOldBooruPage datadir i = do
    rawdoc <- getRawPageBody
            datadir
            old_booru_base_url
            params

    putStrLn $ (show i) ++ " " ++ (hashUrl old_booru_base_url params)
    imageLinks rawdoc

    where
        params = (old_booru_base_params <> "pid" =: i)


fetchOldBooruImagePage :: String -> Option 'Https -> IO ()
fetchOldBooruImagePage datadir params = do
    rawdoc <- getRawPageBody
            datadir
            old_booru_base_url
            params

    print $ renderParams params
    (filename, tags) <- imagePageFilenameTags rawdoc
    putStrLn filename
    mapM_ (putStrLn . ((++) "  ")) tags
    putStrLn ""

-- parse data file
-- leftybooru login?
login :: Url scheme -> Option scheme -> FormUrlEncodedParam -> IO CookieJar
login url params payload = runReq defaultHttpConfig $
    (req
        POST
        url
        (ReqBodyUrlEnc payload)
        ignoreResponse
        params
    )
    >>= liftIO . return . responseCookieJar

mkloginParams :: String -> String -> FormUrlEncodedParam
mkloginParams username password
    =  "user" =: username
    <> "pass" =: password
    <> "submit" =: ("Log+in" :: String)


archiveOldMetadata :: IO ()
archiveOldMetadata = do
    args <- getArgs
    let datadir = head args

    let img_links =
            (mapM (fetchOldBooruPage datadir) idlist) :: IO [[ Option 'Https ]]
    
    img_links >>= (mapM_ (fetchOldBooruImagePage datadir)) . concat

    where
        idlist = [i * 20 | i <- [0..(10800 `quot` 20)]]

main :: IO ()
main = do
    args <- getArgs
    let username = head args
    let password = head $ drop 1 args

    cookies <- login
        new_booru_base_url
        new_booru_login_params
        (mkloginParams username password)

    print cookies
