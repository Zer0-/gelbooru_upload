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
import Control.Exception.Safe (handle)
import Data.Serialize (Serialize (..))
import qualified Crypto.Hash.MD5 as MD5
import Control.Concurrent (threadDelay)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client
    ( defaultRequest
    , CookieJar
    , RequestBody (RequestBodyBS)
    , responseStatus
    )
import Network.HTTP.Types (renderQuery, queryTextToQuery)
import Network.HTTP.Types.Status (Status (statusCode))
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
    , HttpConfig (..)
    , GET (..)
    , POST (..)
    , NoReqBody (..)
    , ReqBodyUrlEnc (..)
    , ReqBodyMultipart
    , reqBodyMultipart
    , FormUrlEncodedParam
    , https
    , http
    -- , port
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
    , HttpException (..)
    )
--import Debug.Trace (trace)
import Text.XML.HXT.DOM.Util (uncurry3)

import Types
    ( Post (..)
    , PostPart (..)
    , Attachment (..)
    )
import Pageparsers
    ( imageLinks
    , imagePageFilenameTags
    , posts
    , threadsInCatalog
    , lainchanFormParams
    , lainchanFirstReply
    , postsInThread
    , flatten
    , FormField (..)
    )
import FSMemoize (fsmemoize)

instance Serialize CookieJar where
    put c = put $ show c
    get = get >>= return . read

type PostWithAttachments scheme = (Post, [ (Url scheme, Maybe String, ByteString) ])

type HttpResponseDat = (Maybe String, ByteString, CookieJar)
type HttpResponseWithMimeAndCookie =
    Either (Int, Maybe ByteString) HttpResponseDat

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

bunkerchan_port :: Option scheme
--bunkerchan_port = port 8080
bunkerchan_port = mempty

-- bunkerchan_root :: Url 'Http
-- bunkerchan_root = http "192.168.4.6" -- "127.0.0.1"

bunkerchan_root :: Url 'Https
bunkerchan_root = https "bunkerchan.xyz"

bunkerchan_leftypol_catalog :: Url 'Https
bunkerchan_leftypol_catalog = bunkerchan_root /: "leftypol" /: "catalog.html"

httpConfig :: HttpConfig
httpConfig = defaultHttpConfig { httpConfigBodyPreviewLength = 1024 * 5 }

httpGetB2 :: Url scheme -> Option scheme -> IO HttpResponseWithMimeAndCookie
httpGetB2 url params = do
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

httpGetB :: Url scheme -> Option scheme -> IO HttpResponseWithMimeAndCookie
httpGetB url params = do
    putStrLn $
        "[GET]"
        ++ show url
        ++ (BS.unpack $ renderParams params)

    handle handler $ httpGetB2 url params

handler :: HttpException -> IO HttpResponseWithMimeAndCookie
handler
    ( VanillaHttpException
        ( HTTP.HttpExceptionRequest
            _ -- Request
            (HTTP.StatusCodeException resp bs)
        )
    ) = return $ Left (statusCode $ responseStatus resp, Just bs)
handler _ = return $ Left (0, Nothing)

cachedGetB :: FilePath -> Url scheme -> Option scheme -> IO HttpResponseWithMimeAndCookie
cachedGetB datadir url params =
    fsmemoize
        datadir
        (uncurry hashUrl)
        (uncurry httpGetB)
        (url, params)

cachedGet :: FilePath -> Url scheme -> Option scheme -> IO (Maybe String, ByteString)
cachedGet datadir url params =
    fsmemoize
        datadir
        (uncurry hashUrl)
        (uncurry httpGet)
        (url, params)

getRawPageBody :: FilePath -> Url scheme -> Option scheme -> IO (Maybe String, ByteString)
getRawPageBody = cachedGet

httpGet :: Url scheme -> Option scheme -> IO (Maybe String, ByteString)
httpGet url params = do
    putStrLn $
        "[GET]"
        ++ show url
        ++ (BS.unpack $ renderParams params)

    runReq httpConfig $ do
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
    :: Maybe String
    -> (ByteString -> IO a)
    -> Url scheme
    -> Option scheme
    -> IO (Either (Int, Maybe ByteString) (a, CookieJar))
fetchAndProcessPageDataU datadir process url params = do
    rsp <- case datadir of
            Just dirname -> cachedGetB dirname url params
            Nothing -> httpGetB url params

    case rsp of
        Left i -> return $ Left i
        Right (_, rawdoc, c) -> do
            result <- process rawdoc

            return $ Right $ (result, c)

    -- rsp >>= \(_, rawdoc, c) -> process rawdoc >>= \result -> return (result, c)

    {-
    (_, rawdoc, c) <- httpGetB
            url
            params

    result <- process rawdoc

    return (result, c)
    -}

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

fetchPostsFromBunkerCatalogPage
    :: Maybe String
    -> Url a
    -> Option a
    -> IO (Maybe [ String ])
fetchPostsFromBunkerCatalogPage datadir url params = do
    e <- fetchAndProcessPageDataU datadir process url params

    case e of
        Left (i, bs) -> do
            putStrLn $ "ERROR getting catalog page " ++ show url ++ " status code " ++ show i ++ " response:"
            print bs
            return Nothing
        Right x -> return $ Just $ fst x

    where
        process rawdoc = do
            print $ renderParams params
            putStrLn "threads on this catalog page:"
            threadPaths <- threadsInCatalog rawdoc
            mapM_ putStrLn threadPaths
            putStrLn ""
            return threadPaths

fetchBunkerchanPostsInThread :: Maybe String -> Url a -> Option a -> IO (Maybe [ Post ])
fetchBunkerchanPostsInThread datadir url params = do
    e <- fetchAndProcessPageDataU datadir process url params

    case e of
        Left i -> do
            putStrLn $ "ERROR getting thread page " ++ show url ++ "! status code: " ++ show i
            return Nothing
        Right x -> return $ Just $ fst x

    -- fetchAndProcessPageDataU process url params >>= return . fst

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
    e <- fetchAndProcessPageDataU Nothing process url params

    case e of
        Left (ie, bs) -> do
            putStrLn $ "GET form page " ++ show url ++ " failed! waiting and retrying."
            putStrLn $ "Status Code: " ++ show ie ++ " \n" ++ show bs
            threadDelay $ 5 * (1000 * 1000)
            fetchLainchanFormPage url params
        Right x -> return x
            

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
    -> PostWithAttachments scheme
    -> [ FormField ]
    -> IO HttpResponseWithMimeAndCookie
postLainchan u o ps ffs =
    handle handler $ runReq httpConfig $ do
        payload <- reqBodyMultipart $ defaultParams ++ (mkLainchanPostParams ps)

        r <- req
            POST
            u
            payload
            bsResponse
            o

        case responseStatusCode r of
            200 -> liftIO $ return $ Right $
                    ( responseHeader r "Content-Type" >>= return . BS.unpack
                    , responseBody r
                    , responseCookieJar r
                    )
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
login_ url params payload = runReq httpConfig $
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

mkLainchanPostParams :: PostWithAttachments scheme -> [ Part ]
mkLainchanPostParams (p, xs) =
    (map (uncurry requestPartFromAttachment) (zip [0..] xs2)) ++
    [ partBS "name" $ encodeUtf8 $ Text.pack $ name p
    , partBS "email" $ encodeUtf8 $ Text.pack $ take 30 $ fromMaybe "" $ email p
    , partBS "subject" $ encodeUtf8 $ Text.pack $ fromMaybe "" $ subject p
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
        -- renderPart (Spoiler ps)       = "[spoiler]" ++ (ps >>= renderPart) ++ "[/spoiler]"
        renderPart (Spoiler ps)       = "**" ++ (ps >>= renderPart) ++ "**"
        renderPart (Bold ps)          = "[b]" ++ (ps >>= renderPart) ++ "[/b]"
        renderPart (Underlined ps)    = "[b]" ++ (ps >>= renderPart) ++ "[/b]"
        renderPart (Italics ps)       = "''" ++ (ps >>= renderPart) ++ "''"
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
    runReq httpConfig $ do
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
 -      - get all html post data (not the attachments!) at once
 -      - build a map :: post number -> Post
 -      - build (Post, deps) tuples from threads :: [[Post]]
 -      - then fold over the tuples with the map as a reference to traverse
 -          the dependancy tree, building a list as we go.
 -      - topo sort requires keeping a set of visited nodes, those can just be
 -          a set of old post ids.
 -      - post the posts in order, but then have to get the new post id and fix
 -      subsequent posts. Rewriting the body.
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


fetchAttachments :: String ->  Post -> IO (Maybe (PostWithAttachments 'Https))
fetchAttachments datadir post2 = do
    saved <-
        ( mapM
            ( \a ->
                let u = mkBnkrUrl $ prepareUrlStr $ attachmentUrl a
                {-
                 - TODO: Ban image urls here somewhere!
                 -
                 - Also the result of this "saved" will be a Maybe.
                 - This needs to be zipped with the Attachments list in posts2
                 - Then return posts2 with the  attachments list filtered
                 - on the gotten attachment value not being Nothing
                 -
                 - and the attachment list flattened (looks like it already is)
                 -}
                in
                    (getFn u)
                    >>=
                    ((\p ->
                        case p of
                            Left i -> do
                                putStrLn $ "Could not fetch attachment " ++ show u
                                    ++ " status code: " ++ show i
                                return Nothing
                            Right (m, b, _) -> return $ Just (u, m, b)
                    ) :: HttpResponseWithMimeAndCookie -> IO (Maybe (Url 'Https, Maybe String, ByteString)))
                    -- >>= \(m, b, _) -> return (u, m, b)
            )
            (attachments post2)
        )

    let saved1 = flatten saved

    if length saved1 == 0 && length (postBody post2) == 0
    then return Nothing
    else return $ Just (post2, saved1)
    -- should be a post3 here

    where
        getFn = (flip (cachedGetB datadir)) bunkerchan_port

        mkBnkrUrl = mkUrl bunkerchan_root

        prepareUrlStr = Text.pack . (drop 1)


processThread :: String -> [ Post ] -> IO ()
processThread datadir thread = do
    posts2a <- mapM (fetchAttachments datadir) thread

    let posts2 = filter dropUnknownFiles $ filter dropUnknownMime (flatten posts2a)

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

            putStrLn "Posting OP:"
            print $ (\(p, _) -> (postNumber p, subject p, attachments p)) (head posts2)

            reply <- postLainchan
                lainchan_post_url
                (header "Referer" (BS.pack $ "http://" ++ lainchan_ip ++ "/b/index.html"))
                (head posts2)
                ss

            case reply of
                Left (ie, bs) -> do
                        putStrLn $ "POST  failed! Status Code: " ++ show ie
                        putStrLn $ show bs
                        processThread datadir $ drop 1 thread
                Right (_, rawreply, _) -> do
                    threadUrlStr <- lainchanFirstReply rawreply

                    putStrLn threadUrlStr

                    mapM_
                        ( \p -> do
                            (ss2, _) <- fetchLainchanFormPage
                                (mkUrl lainchan_base $ Text.pack (drop 1 threadUrlStr))
                                mempty

                            mapM_ print ss2

                            putStrLn "Posting Reply:"
                            print $ (\(p2, _) -> (postNumber p2, subject p2, attachments p2)) p

                            result <- postLainchan
                                lainchan_post_url
                                (header "Referer" (BS.pack $ "http://" ++ lainchan_ip ++ threadUrlStr))
                                p
                                ss2

                            case result of
                                Left (ie, bs) -> do
                                    putStrLn $ "POST  failed! Status Code: " ++ show ie
                                    print bs
                                    return ()
                                Right _ -> return ()
                        )
                        (drop 1 posts2)

    where
        dropUnknownMime :: PostWithAttachments scheme -> Bool
        dropUnknownMime (_, xs) = filter (\(_, m, _) -> badMime m) xs == []

        {-
        badMime (Just "video/mp4") = True
        badMime (Just "video/webm") = True
        -}
        badMime _ = False

        dropUnknownFiles :: PostWithAttachments scheme -> Bool
        dropUnknownFiles (p, _) =
            filter badFile (map attachmentUrl (attachments p)) == []

        badFile "/.media/8745619976e83ad3ea484f2aa3507c4f-imagepng.png" = True
        badFile "/.media/52157a7ed00858d44ae0fab6a265bc27-imagepng.png" = True
        badFile _ = False


mkUrl :: Url a -> Text.Text -> Url a
mkUrl root = (foldl (/:) root) . (Text.splitOn "/")

main :: IO ()
main = do
    args <- getArgs
    let datadir = head args

    putStrLn $ "datadir: " ++ datadir

    mThreadPaths <- fetchPostsFromBunkerCatalogPage
            (Just datadir)
            bunkerchan_leftypol_catalog
            bunkerchan_port

    threads1 <- case mThreadPaths of
        Nothing -> return []
        Just threadPaths ->
            mapM
                ( \u ->
                    ((flip (fetchBunkerchanPostsInThread $ Just datadir)) bunkerchan_port) $
                        mkUrl bunkerchan_root u
                )
                (map (Text.pack . (drop 1)) $ reverse threadPaths)

    let threads = (flatten threads1) :: [[ Post ]] -- ✓

    {-
     - So now what?
     -  ok Have? [[ Post ]]
     -  WANT? Map Int Post
     -  call it ? postsByNum
     -  call function? indexPosts
     -}

    putStrLn $ "have " ++ (show $ length threads) ++ " threads!"

    mapM_
        printPostPartQuote
        (concat threads >>= postBody)

    putStrLn "Done"

    where
        printPostPartQuote :: PostPart -> IO ()
        printPostPartQuote (Quote s) = putStrLn s
        printPostPartQuote _ = return ()


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
