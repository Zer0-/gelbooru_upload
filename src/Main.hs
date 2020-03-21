{-# LANGUAGE OverloadedStrings, DataKinds #-}

module Main where

import System.Environment (getArgs)
import System.FilePath (FilePath, (</>))
import qualified Data.Map as Map
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.List.Split (splitWhen, splitOn)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64.URL as BSURL
import Data.Semigroup (Endo (..))
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Hash.MD5 as MD5
import Network.HTTP.Client (defaultRequest, CookieJar)
import Network.HTTP.Types (renderQuery, queryTextToQuery)
import Network.HTTP.Client.MultipartFormData
    ( partBS
    , partContentType
    , partFileSource
    )
import Network.HTTP.Req
    ( runReq
    , req
    , Req
    , defaultHttpConfig
    , GET (..)
    , POST (..)
    , NoReqBody (..)
    , ReqBodyUrlEnc (..)
    , ReqBodyMultipart
    , reqBodyMultipart
    , FormUrlEncodedParam
    , https
    , http
    , bsResponse
    , ignoreResponse
    , responseBody
    , responseStatusCode
    , responseCookieJar
    , cookieJar
    , (=:) --query params
    , (/:)
    , Option (Option)
    , Url
    , Scheme (..)
    )
import Debug.Trace (trace)
import Text.XML.HXT.DOM.Util (uncurry3)

import Pageparsers (imageLinks, imagePageFilenameTags)
import FSMemoize (fsmemoize)

-- login page (new): https://leftypol.booru.org/index.php?page=login
-- posts page (old): https://lefty.booru.org/index.php?page=post&s=list&tags=all&pid=0

old_booru_base_url :: Url 'Https
old_booru_base_url = https "lefty.booru.org" /: "index.php"

new_booru_base_url :: Url 'Https
new_booru_base_url = https "leftypol.booru.org" /: "index.php"

new_booru_base_url_plaintext :: Url 'Http
new_booru_base_url_plaintext = http "leftypol.booru.org" /: "index.php"

old_booru_base_params :: Option 'Https
old_booru_base_params
    =  "page" =: ("post" :: String)
    <> "s" =: ("list" :: String)
    <> "tags" =: ("all" :: String)

new_booru_login_params :: Option 'Https
new_booru_login_params
    =  "page" =: ("login" :: String)
    <> "code" =: ("00" :: String)

new_booru_post_params :: Option 'Https
new_booru_post_params
    =  "page" =: ("post" :: String)
    <> "s" =: ("add" :: String)

getRawPageBody_ :: Url scheme -> Option scheme -> IO ByteString
getRawPageBody_ url params = runReq defaultHttpConfig $ do
    r <- req
        GET
        url
        NoReqBody
        bsResponse
        params

    liftIO $ return $ responseBody r

getRawPageBody :: FilePath -> Url scheme -> Option scheme -> IO ByteString
getRawPageBody datadir url params =
    fsmemoize
        datadir
        (uncurry hashUrl)
        (uncurry getRawPageBody_)
        (url, params)

renderParams :: Option scheme -> ByteString
renderParams (Option f _) = renderQuery True (queryTextToQuery params)
    where
        params = fst $ appEndo f ([], defaultRequest)

hash :: ByteString -> String
hash = BS.unpack . BSURL.encode . MD5.hash

urlToBS :: Url scheme -> Option scheme -> ByteString
urlToBS url params = BS.concat
    [ BS.pack (show url)
    , renderParams params
    ]

hashUrl :: Url scheme -> Option scheme -> String
hashUrl url params = hash $ urlToBS url params


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

login_ :: Url scheme -> Option scheme -> FormUrlEncodedParam -> IO CookieJar
login_ url params payload = runReq defaultHttpConfig $
    (req
        POST
        url
        (ReqBodyUrlEnc payload)
        ignoreResponse
        params
    )
    >>= liftIO . return . responseCookieJar

login :: FilePath -> Url scheme -> Option scheme -> FormUrlEncodedParam -> IO CookieJar
login datadir url params payload =
    fsmemoize
        datadir
        (\(u, option, _) -> hashUrl u option)
        (uncurry3 login_)
        (url, params, payload)


mkloginParams :: String -> String -> FormUrlEncodedParam
mkloginParams username password
    =  "user" =: username
    <> "pass" =: password
    <> "submit" =: ("Log+in" :: String)

mkPostParams
    :: Map.Map FilePath String
    -> FilePath
    -> String
    -> [String]
    -> Req ReqBodyMultipart
mkPostParams mime imgdir filename tags = reqBodyMultipart
    [ let
        upload = "upload"
        part =
            partFileSource
                upload
                filepath
        part2 = part
                { partContentType = Just $
                    BS.pack $ Map.findWithDefault "" filepath mime
                }
        in
            part2
            --trace ("Content-Type: " ++ (show $ partContentType part2) ++ "\n" ++ show filepath) part2

    , partBS "source" ""
    , partBS "title" ""
    , partBS "tags" $ BS.pack (intercalate " " [ replaceSpace t | t <- tags])
    , partBS "rating" "q"
    , partBS "submit" "Upload"
    ]

    where
        replaceSpace " " = "_"
        replaceSpace x = x

        filepath = imgdir </> filename


post_
    :: Map.Map FilePath String
    -> Url scheme
    -> Option scheme
    -> FilePath
    -> CookieJar
    -> String
    -> [String]
    -> IO ByteString
post_ mime url params imgdir cookies filename tags = runReq defaultHttpConfig $ do
    payload <- mkPostParams mime imgdir filename tags

    r <- req
        POST
        url
        payload
        bsResponse
        (params <> cookieJar cookies)

    case responseStatusCode r of
        200 -> liftIO $ return $ responseBody r
        _ -> error "Upload failed"

-- insane just make a data definition!
-- why am i adding to this!!!
uncurry7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> (a, b, c, d, e, f, g) -> h
uncurry7 h (a, b, c, d, e, f, g) = h a b c d e f g

hashPostArgs
    :: c
    -> Url scheme
    -> Option scheme
    -> a
    -> b
    -> String
    -> [String]
    -> String
hashPostArgs _ url params _ _ filename tags =
    hash $ BS.concat
        [ urlToBS url params
        , BS.pack filename
        , BS.pack $ concat tags
        ]

post
    :: Map.Map FilePath String
    -> FilePath
    -> Url scheme
    -> Option scheme
    -> FilePath
    -> CookieJar
    -> String
    -> [String]
    -> IO ByteString
post mime datadir url params imgdir cookies filename tags =
    fsmemoize
        datadir
        (uncurry7 hashPostArgs)
        (uncurry7 post_)
        (mime, url, params, imgdir, cookies, filename, tags)


archiveOldMetadata :: IO ()
archiveOldMetadata = do
    args <- getArgs
    let datadir = head args

    let img_links =
            (mapM (fetchOldBooruPage datadir) idlist) :: IO [[ Option 'Https ]]
    
    img_links >>= (mapM_ (fetchOldBooruImagePage datadir)) . concat

    where
        idlist = [i * 20 | i <- [0..(10800 `quot` 20)]]

parseFileList :: String -> [(String, [String])]
parseFileList
    = (map (\(x:xs) -> (x, xs)))
    . (splitWhen ((==) ""))
    . lines

parseMimeFile :: String -> Map.Map FilePath String
parseMimeFile
    = Map.fromList
    . (map (\(filename : mimetype : _) -> (init filename, init mimetype)))
    . (map (splitOn " "))
    . (drop 1)
    . lines

main :: IO ()
main = do
    args <- getArgs
    let datadir = head args
    let username = head $ drop 1 args
    let password = head $ drop 2 args
    let tags_filename = head $ drop 3 args
    let pictures_dir = head $ drop 4 args
    let images_mime_file = head $ drop 5 args

    putStrLn $ "login cookie jar filename: " ++ hashUrl new_booru_base_url new_booru_login_params

    cookies <- login
        datadir
        new_booru_base_url
        new_booru_login_params
        (mkloginParams username password)

    print cookies

    tagsdata <- readFile tags_filename
    mime_types <- readFile images_mime_file >>= return . parseMimeFile

    mapM_
        ( \(filename, tags) -> do
            putStrLn $ filename ++ " " ++ hashPostArgs
                    mime_types
                    new_booru_base_url
                    new_booru_post_params
                    pictures_dir
                    cookies
                    filename
                    tags

            post
                mime_types
                datadir
                new_booru_base_url
                new_booru_post_params
                pictures_dir
                cookies
                filename
                tags
        )
        (parseFileList tagsdata)
        --[head (parseFileList tagsdata)]
