{-# LANGUAGE OverloadedStrings, DataKinds, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- There is an orphan instance of CookieJar which we need to not login every request
-- I don't consider this a hack, CookieJar should derive something like this already

module Main where

import System.Environment (getArgs)
import System.FilePath (FilePath, (</>))
import qualified Data.Map as Map
import Data.ByteString (ByteString, empty)
import Data.List (intercalate, sort)
import Data.List.Split (splitWhen, splitOn)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64.URL as BSURL
import Data.Semigroup (Endo (..))
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (encodeUtf8)
import Control.Monad.IO.Class (liftIO)
import Data.Serialize (Serialize (..))
import qualified Crypto.Hash.MD5 as MD5
import Network.HTTP.Client
    ( defaultRequest
    , CookieJar
    , RequestBody (RequestBodyBS)
    )
import Network.HTTP.Types (renderQuery, queryTextToQuery)
import Network.HTTP.Client.MultipartFormData
    ( partBS
    , partContentType
    , partFileSource
    , partFileRequestBody
    , Part
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
    , responseHeader
    , responseStatusCode
    , responseCookieJar
    , cookieJar
    , header
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
    , lainchanFormParams
    , lainchanFirstReply
    , postsInThread
    , Post (..)
    , PostPart (..)
    , Attachment (..)
    , FormField (..)
    )
import FSMemoize (fsmemoize)

instance Serialize CookieJar where
    put c = put $ show c
    get = get >>= return . read

type PostWithAttachments = (Post, [ (Url 'Https, Maybe String, ByteString) ])

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

lainchan_ip :: String
lainchan_ip = "167.99.9.53"

lainchan_base :: Url 'Http
lainchan_base = http $ Text.pack lainchan_ip

lainchan_b :: Url 'Http
lainchan_b = lainchan_base /: "b" /: "index.html"

lainchan_post_url :: Url 'Http
lainchan_post_url = lainchan_base/: "post.php"

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

bunkerchan_port :: Int
bunkerchan_port = 80

-- bunkerchan_root :: Url 'Http
-- bunkerchan_root = http "192.168.4.6" -- "127.0.0.1"

bunkerchan_root :: Url 'Https
bunkerchan_root = https "bunkerchan.xyz"

bunkerchan_leftypol_catalog :: Url 'Https
bunkerchan_leftypol_catalog = bunkerchan_root /: "leftypol" /: "catalog.html"

httpGetB :: Url scheme -> Option scheme -> IO (Maybe String, ByteString, CookieJar)
httpGetB url params = do
    putStrLn $
        "[GET]"
        ++ show url
        ++ (BS.unpack $ renderParams params)

    runReq defaultHttpConfig $ do
        r <- req
            GET
            url
            NoReqBody
            bsResponse
            params

        liftIO $ return $
            ( responseHeader r "Content-Type" >>= return . BS.unpack
            , responseBody r
            , responseCookieJar r
            )

httpGet :: Url scheme -> Option scheme -> IO (Maybe String, ByteString)
httpGet url params = do
    putStrLn $
        "[GET]"
        ++ show url
        ++ (BS.unpack $ renderParams params)

    runReq defaultHttpConfig $ do
        r <- req
            GET
            url
            NoReqBody
            bsResponse
            params

        liftIO $ return $
            ( responseHeader r "Content-Type" >>= return . BS.unpack
            , responseBody r
            )

cachedGet :: FilePath -> Url scheme -> Option scheme -> IO (Maybe String, ByteString)
cachedGet datadir url params =
    fsmemoize
        datadir
        (uncurry hashUrl)
        (uncurry httpGet)
        (url, params)

getRawPageBody :: FilePath -> Url scheme -> Option scheme -> IO (Maybe String, ByteString)
getRawPageBody = cachedGet


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
    (_, rawdoc) <- getRawPageBody
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
    -> IO (a, CookieJar)
fetchAndProcessPageDataU process url params = do
    (_, rawdoc, c) <- httpGetB
            url
            params

    result <- process rawdoc

    return (result, c)

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
    fetchAndProcessPageDataU process url params >>= return . fst

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
    fetchAndProcessPageDataU process url params >>= return . fst

    where
        process rawdoc = do
            --print $ renderParams params
            putStrLn "posts in this thread:"
            -- print rawdoc
            postss <- postsInThread rawdoc
            mapM_ print postss
            putStrLn ""
            return postss

fetchLainchanFormPage :: Url a -> Option a -> IO ([ FormField ], CookieJar)
fetchLainchanFormPage url params = do
    fetchAndProcessPageDataU process url params

    where
        process rawdoc = do
            print $ renderParams params
            putStrLn "parameters on this login page:"
            existingParams <- lainchanFormParams rawdoc
            --mapM_ putStrLn existingParams
            return existingParams

postLainchan
    :: Url a
    -> Option a
    -> PostWithAttachments
    -> [ FormField ]
    -> IO ByteString
postLainchan u o ps ffs =
    runReq defaultHttpConfig $ do
        payload <- reqBodyMultipart $ defaultParams ++ (mkLainchanPostParams ps)

        r <- req
            POST
            u
            payload
            bsResponse
            o

        case responseStatusCode r of
            200 -> liftIO $ return $ responseBody r
            e -> error $ "Upload failed! result code " ++ show e

     where
        defaultParams = map formFieldToParam (filter isDefault ffs)

        isDefault (FormField { fieldName = "name" })    = False
        isDefault (FormField { fieldName = "email" })   = False
        isDefault (FormField { fieldName = "subject" }) = False
        isDefault (FormField { fieldName = "body" })    = False
        isDefault (FormField { fieldName = "file" })    = False
        isDefault _ = True

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

mkLainchanPostParams :: PostWithAttachments -> [ Part ]
mkLainchanPostParams (p, xs) =
    (map (uncurry requestPartFromAttachment) (zip [0..] xs2)) ++
    [ partBS "name" $ BS.pack (name p)
    , partBS "email" $ BS.pack (fromMaybe "" $ email p)
    , partBS "subject" $ BS.pack (fromMaybe "" $ subject p)
    , partBS "body" $ encodeUtf8 $ Text.pack $ renderPostBody $ postBody p
    ]

     where
        xs2 = map (\(a, (_, b, c)) -> (a, b, c)) (zip (attachments p) xs)

formFieldToParam :: FormField -> Part
formFieldToParam (FormField { fieldName, fieldValue }) =
    partBS (Text.pack fieldName) $ encodeUtf8 $ Text.pack fieldValue

renderPostBody :: [ PostPart ] -> String
renderPostBody = ((=<<) :: (PostPart -> String) -> [ PostPart ] ->  String) renderPart
    where
        renderPart :: PostPart -> String
        renderPart (SimpleText s) = s
        renderPart (PostedUrl s) = s
        renderPart Skip = ""
        renderPart (Quote s) = s
        renderPart (GreenText ps)     = ps >>= renderPart
        renderPart (OrangeText ps)    = ps >>= renderPart
        renderPart (RedText ps)       = ps >>= renderPart
        renderPart (Spoiler ps)       = "[spoiler]" ++ (ps >>= renderPart) ++ "[/spoiler]"
        renderPart (Bold ps)          = "[b]" ++ (ps >>= renderPart) ++ "[/b]"
        renderPart (Underlined ps)    = "[b]" ++ (ps >>= renderPart) ++ "[/b]"
        renderPart (Italics ps)       = "[i]" ++ (ps >>= renderPart) ++ "[/i]"
        renderPart (Strikethrough ps) = "[i]" ++ (ps >>= renderPart) ++ "[/i]"

requestPartFromAttachment :: Int -> (Attachment, Maybe String, ByteString) -> Part
requestPartFromAttachment i (a, m, bs) =
    ( partFileRequestBody
        (fieldName i)
        (attachmentFilename a)
        (RequestBodyBS bs)
    ) { partContentType = m >>= Just . BS.pack }

    where
        fieldName 0 = "file"
        fieldName j = Text.pack $ "file" ++ show j


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
post_ mime url params imgdir cookies filename tags =
    runReq defaultHttpConfig $ do
        payload <- mkPostParams mime imgdir filename tags

        r <- req
            POST
            url
            payload
            bsResponse
            (params <> cookieJar cookies)

        case responseStatusCode r of
            200 -> liftIO $ return $ responseBody r
            e -> error $ "Upload failed! result code " ++ show e

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
 -
 -      - GET board page from lainchan (to post op)
 -      - parse parameters
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


fetchAttachments :: String ->  Post -> IO PostWithAttachments
fetchAttachments datadir post2 = do
    saved <- mapM
        ( \a ->
            let u = mkBnkrUrl $ prepareUrlStr $ attachmentUrl a
            in
                (getFn u)
                >>= \(m, b) -> return (u, m, b)
        )
        (attachments post2)

    return (post2, saved)

    where
        getFn = (flip (cachedGet datadir)) (port bunkerchan_port)

        mkBnkrUrl = mkUrl bunkerchan_root

        prepareUrlStr = Text.pack . (drop 1)

optFromFormField :: FormField -> Option a
optFromFormField = undefined

processThread :: String -> [ Post ] -> IO ()
processThread datadir thread = do
    posts2a <- mapM (fetchAttachments datadir) thread

    let posts2 = filter dropUnknownFiles posts2a

    -- this just prints the thread
    mapM_ (\(p, xs) -> do
        print (postNumber p)
        mapM_ (\(u, m, _) -> putStrLn $ " " ++ show (u, m)) xs
        putStrLn "")
        posts2

    (ss, c) <- fetchLainchanFormPage
        lainchan_b
        mempty

    putStrLn "current cookie jar:"
    print c

    mapM_ print ss

    case posts2 of
        [] -> return ()
        _ -> do
            -- post OP
            reply <- postLainchan
                lainchan_post_url
                (header "Referer" (BS.pack $ "http://" ++ lainchan_ip ++ "/b/index.html"))
                (head posts2)
                ss

            print reply

            putStrLn ""

            threadUrlStr <- lainchanFirstReply reply

            putStrLn threadUrlStr

            mapM_
                ( \p -> do
                    (ss2, _) <- fetchLainchanFormPage
                        (mkUrl lainchan_base $ Text.pack (drop 1 threadUrlStr))
                        mempty

                    mapM_ print ss

                    reply2 <- postLainchan
                        lainchan_post_url
                        (header "Referer" (BS.pack $ "http://" ++ lainchan_ip ++ threadUrlStr))
                        p
                        ss2

                    print reply2
                )
                (drop 1 posts2)

    where
        dropUnknownFiles :: PostWithAttachments -> Bool
        dropUnknownFiles (_, xs) = filter (\(_, m, _) -> badMime m) xs == []

        badMime (Just "video/mp4") = True
        badMime (Just "video/webm") = True
        badMime _ = False


mkUrl :: Url a -> Text.Text -> Url a
mkUrl root = (foldl (/:) root) . (Text.splitOn "/")

main :: IO ()
main = do
    args <- getArgs
    let datadir = head args

    putStrLn $ "datadir: " ++ datadir

    threadPaths <- fetchPostsFromBunkerCatalogPage
            bunkerchan_leftypol_catalog
            (port bunkerchan_port)

    posts2 <-
        ( mapM
            (((flip fetchBunkerchanPostPage) (port bunkerchan_port)) . (mkUrl bunkerchan_root))
            (map (Text.pack . (drop 1)) threadPaths)
        ) :: IO [[ Post ]]

    mapM_ (processThread datadir) posts2

    putStrLn "Done"


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
