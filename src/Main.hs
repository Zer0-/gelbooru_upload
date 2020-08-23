{-# LANGUAGE OverloadedStrings, DataKinds, NamedFieldPuns #-}

module Main where

import System.Environment (getArgs)
import System.FilePath (FilePath, (</>))
import qualified Data.Map as Map
import Data.ByteString (ByteString, empty)
import Data.List (intercalate, sort)
import Data.List.Split (splitWhen, splitOn)
import qualified Data.Text
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
    , port
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
--import Debug.Trace (trace)
import Text.XML.HXT.DOM.Util (uncurry3)

import Pageparsers
    ( imageLinks
    , imagePageFilenameTags
    , posts
    , threadsInCatalog
    , postsInThread
    , Post (..)
    , Attachment (..)
    )
import FSMemoize (fsmemoize)

-- login page (new): https://leftypol.booru.org/index.php?page=login
-- posts page (old): https://lefty.booru.org/index.php?page=post&s=list&tags=all&pid=0

old_booru_base_url :: Url 'Https
old_booru_base_url = https "lefty.booru.org" /: "index.php"

new_booru_base_url :: Url 'Https
new_booru_base_url = https "leftypol.booru.org" /: "index.php"

new_new_booru_base_url :: Url 'Https
new_new_booru_base_url = https "leftypics.booru.org" /: "index.php"

new_booru_edit_url :: Url 'Https
new_booru_edit_url = https "leftypol.booru.org" /: "public" /: "edit_post.php"

new_booru_base_url_plaintext :: Url 'Http
new_booru_base_url_plaintext = http "leftypol.booru.org" /: "index.php"

new_booru_delete_url :: Url 'Https
new_booru_delete_url = https "leftypol.booru.org" /: "public" /: "remove.php"

booru_base_params :: Option 'Https
booru_base_params
    =  "page" =: ("post" :: String)
    <> "s" =: ("list" :: String)
    <> "tags" =: ("all" :: String)

booru_login_params :: Option 'Https
booru_login_params
    =  "page" =: ("login" :: String)
    <> "code" =: ("00" :: String)

booru_post_params :: Option 'Https
booru_post_params
    =  "page" =: ("post" :: String)
    <> "s" =: ("add" :: String)

booru_view_id_params :: Option 'Https
booru_view_id_params
    =  "page" =: ("post" :: String)
    <> "s" =: ("view" :: String)
    --"index.php?page=post&s=view&id=1200

delete_post_params :: Int -> Option 'Https
delete_post_params i
    = "id" =: i
    <> "removepost" =: (1 :: Int)
            -- ./public/remove.php?id=11204&amp;removepost=1

bunkerchan_root :: Url 'Http
bunkerchan_root = http "127.0.0.1"

bunkerchan_leftypol_catalog :: Url 'Http
bunkerchan_leftypol_catalog = bunkerchan_root /: "leftypol" /: "catalog.html"

fetchPageDataU :: Url scheme -> Option scheme -> IO ByteString
fetchPageDataU url params = runReq defaultHttpConfig $ do
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
        (uncurry fetchPageDataU)
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


fetchPageData
    :: String
    -> (ByteString -> IO a)
    -> Url scheme
    -> Option scheme
    -> IO a
fetchPageData datadir process url params = do
    rawdoc <- getRawPageBody
            datadir
            url
            params

    putStrLn $
        show url
        ++ (BS.unpack $ renderParams params)
        ++ " " ++ hashUrl url params

    process rawdoc


fetchAndProcessPageDataU
    :: (ByteString -> IO a)
    -> Url scheme
    -> Option scheme
    -> IO a
fetchAndProcessPageDataU process url params = do
    rawdoc <- fetchPageDataU
            url
            params

    putStrLn $
        "[GET]"
        ++ show url
        ++ (BS.unpack $ renderParams params)

    process rawdoc

fetchBooruPostPage
    :: String
    -> Url 'Https
    -> Int
    -> IO [ Option 'Https ]
fetchBooruPostPage datadir url i =
    fetchPageData datadir imageLinks url params

    where
        params = booru_base_params <> "pid" =: i


fetchBooruImagePage :: String -> Url a -> Option a -> IO ()
fetchBooruImagePage datadir url params =
    fetchPageData datadir process url params

    where
        process rawdoc = do
            print $ renderParams params
            (filename, tags) <- imagePageFilenameTags rawdoc
            putStrLn filename
            mapM_ (putStrLn . ((++) "  ")) tags
            putStrLn ""

fetchPostsFromBunkerCatalogPage :: Url a -> Option a -> IO [ String ]
fetchPostsFromBunkerCatalogPage url params =
    fetchAndProcessPageDataU process url params

    where
        process rawdoc = do
            print $ renderParams params
            putStrLn "threads on this catalog page:"
            threadPaths <- threadsInCatalog rawdoc
            mapM_ putStrLn threadPaths
            putStrLn ""
            return threadPaths

fetchBunkerchanPostPage :: Url a -> Option a -> IO [ Post ]
fetchBunkerchanPostPage url params =
    fetchAndProcessPageDataU process url params

    where
        process rawdoc = do
            --print $ renderParams params
            putStrLn "posts in this thread:"
            -- print rawdoc
            postss <- postsInThread rawdoc
            mapM_ print postss
            putStrLn ""
            return postss

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


archiveMetadata :: IO ()
archiveMetadata = do
    args <- getArgs
    let datadir = head args

    let img_links =
            (mapM (fetchBooruPostPage datadir new_booru_base_url) idlist) :: IO [[ Option 'Https ]]
    
    img_links
        >>= (mapM_ (fetchBooruImagePage datadir new_booru_base_url))
        . concat

    where
        idlist = [i * 20 | i <- [0..(10800 `quot` 20)]]

userPosts
    :: String
    -> String
    -> (ByteString -> IO a)
    -> Int
    -> IO a
userPosts datadir username process i = do
    fetchPageData datadir process new_booru_base_url params

    where
        params
            = "page" =: ("post" :: String)
            <> "s" =: ("list" :: String)
            <> "tags" =: ("user:" ++ username :: String)
            <> "pid" =: i

allUserPosts :: String -> String -> IO [(Int, String)]
allUserPosts datadir username = do
    posts_ <- mapM
        (userPosts datadir username posts)
        [i * 20 | i <- [0..(10180 `quot` 20)]]

    return (concat posts_)

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

{-
 - TODO:
 -      - get memoized file data
 -      - post on lainchan
 -          - PostPart -> String
 -          - file upload (this should be working already hopefully)
 -}

main_old :: IO ()
main_old = do
    args <- getArgs
    let datadir = head args
    let username = head $ drop 1 args
    let password = head $ drop 2 args
    let tags_filename = head $ drop 3 args
    let pictures_dir = head $ drop 4 args
    let images_mime_file = head $ drop 5 args

    putStrLn $ "login cookie jar filename: " ++ hashUrl new_new_booru_base_url booru_login_params

    cookies <- login
        datadir
        new_new_booru_base_url
        booru_login_params
        (mkloginParams username password)

    print cookies

    tagsdata <- readFile tags_filename
    mime_types <- readFile images_mime_file >>= return . parseMimeFile

    mapM_
        ( \(filename, tags) -> do
            putStrLn $ filename ++ " " ++ hashPostArgs
                    mime_types
                    new_new_booru_base_url
                    booru_post_params
                    pictures_dir
                    cookies
                    filename
                    tags

            post
                mime_types
                datadir
                new_new_booru_base_url
                booru_post_params
                pictures_dir
                cookies
                filename
                tags
        )
        (parseFileList tagsdata)
        --(take 100 (parseFileList tagsdata))
        --[head $ parseFileList tagsdata]


processThread :: [ Post ] -> IO ()
processThread = print . fileUrls
    where
        fileUrls = (=<<) attachmentsInPost

        attachmentsInPost :: Post -> [ String ]
        attachmentsInPost (Post { attachments }) = map attachmentUrl attachments


main :: IO ()
main = do
    putStrLn "Hello World"

    args <- getArgs
    let datadir = head args

    putStrLn datadir

    threadPaths <- fetchPostsFromBunkerCatalogPage
            bunkerchan_leftypol_catalog
            (port 8080)

    posts2 <-
        ( mapM
            (((flip fetchBunkerchanPostPage) (port 8080)) . mkBunkerchanThreadUrl)
            (map (Data.Text.pack . (drop 1)) threadPaths)
        ) :: IO [[ Post ]]

    -- mapM_ ((flip (getRawPageBody undefined)) (port 8080)) (
    mapM_ processThread posts2

    putStrLn "Done"

    where
        mkBunkerchanThreadUrl = (foldl (/:) bunkerchan_root) . (Data.Text.splitOn "/")
        --mkBunkerchanThreadUrl = (/:) bunkerchan_root



mkTagUpdatePostParams
    :: Int
    -> [ String ]
    -> ReqBodyUrlEnc
mkTagUpdatePostParams i tags = ReqBodyUrlEnc $
    "rating"           =: ("q" :: String)
    <> "title"         =: ("" :: String)
    <> "parent"        =: ("" :: String)
    <> "next_post"     =: ("" :: String)
    <> "previous_post" =: ("" :: String)
    <> "source"        =: ("" :: String)
    <> "tags"          =: intercalate " " tags
    <> "pconf"         =: (1 :: Int)
    <> "id"            =: show i
    <> "submit"        =: ("Save+changes" :: String)

postTagUpdate_
    :: Int
    -> [ String ]
    -> CookieJar
    -> IO ByteString
postTagUpdate_ i tags cookies = runReq defaultHttpConfig $ do
    r <- req
        POST
        new_booru_edit_url
        (mkTagUpdatePostParams i tags)
        bsResponse
        (cookieJar cookies)

    case responseStatusCode r of
        200 -> liftIO $ return $ responseBody r
        _ -> error "Upload failed"

postTagUpdate
    :: String
    -> Int
    -> [ String ]
    -> CookieJar
    -> IO ByteString
postTagUpdate datadir i tags cookies = do
    putStrLn $ "update tags " ++ hashTagUpdate i tags cookies
    putStrLn ""

    fsmemoize
        datadir
        (uncurry3 hashTagUpdate)
        (uncurry3 postTagUpdate_)
        (i, tags, cookies)

    where
        hashTagUpdate i_ tags_ _ = hash $ BS.concat
            [ BS.pack $ show new_booru_edit_url
            , BS.pack $ show i_
            , BS.pack $ concat (sort tags_)
            ]

reTag :: IO ()
reTag = do
    args <- getArgs
    let datadir = head args
    let username = head $ drop 1 args
    let password = head $ drop 2 args
    let tags_filename = head $ drop 3 args

    putStrLn $ "login cookie jar filename: " ++ hashUrl new_booru_base_url booru_login_params

    cookies <- login
        datadir
        new_booru_base_url
        booru_login_params
        (mkloginParams username password)

    print cookies

    tagsdata <- readFile tags_filename

    let tagmap = Map.fromList $ map
            (\(f, tgs) -> (last $ splitOn "/" f, tgs))
            (parseFileList tagsdata)

    posts_ <- allUserPosts datadir username
    print $ length posts_

    mapM_
        (\(i, filename) -> do
            putStrLn filename
            let tags = Map.findWithDefault [] filename tagmap
            putStrLn $
                (show i) ++ " " ++ filename ++ " " ++ (intercalate " " tags)
            postTagUpdate datadir i tags cookies
        )
        posts_

removePost_
    :: Int
    -> CookieJar
    -> IO ByteString
removePost_ i cookies = runReq defaultHttpConfig $ do
    r <- req
        POST
        new_booru_delete_url
        NoReqBody
        ignoreResponse
        (delete_post_params i <> cookieJar cookies)

    case responseStatusCode r of
        200 -> liftIO $ return $ empty
        _ -> error "Delete failed"

removePost
    :: String
    -> Int
    -> CookieJar
    -> IO ()
removePost datadir i cookies = do
    putStrLn $ "Delete " ++ show i ++ " " ++ h

    _ <- fsmemoize
        datadir
        (const h)
        (const $ removePost_ i cookies)
        (i, cookies)

    return ()

    where
        h = hashUrl new_booru_delete_url (delete_post_params i)
        

deletePosts :: IO ()
deletePosts = do
    args <- getArgs
    let datadir = head args
    let username = head $ drop 1 args
    let password = head $ drop 2 args

    putStrLn $ "login cookie jar filename: " ++ hashUrl new_booru_base_url booru_login_params

    cookies <- login
        datadir
        new_booru_base_url
        booru_login_params
        (mkloginParams username password)

    print cookies


    posts_ <- allUserPosts datadir username

    mapM_
        (\(i, _) -> removePost datadir i cookies)
        posts_
