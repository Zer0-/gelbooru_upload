{-# LANGUAGE OverloadedStrings, DataKinds, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- There is an orphan instance of CookieJar which we need to not login every request
-- I don't consider this a hack, CookieJar should derive something like this already

module Main
    ( processThread
    , main
    ) where

import System.Environment (getArgs)
import Data.ByteString (ByteString)
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
    , partFileRequestBody
    , Part
    )
import Network.HTTP.Req
    ( runReq
    , req
    , defaultHttpConfig
    , HttpConfig (..)
    , GET (..)
    , POST (..)
    , NoReqBody (..)
    , reqBodyMultipart
    , https
    , http
    -- , port
    , bsResponse
    , responseBody
    , responseHeader
    , responseStatusCode
    , responseCookieJar
    , header
    , (/:)
    , Option (Option)
    , Url
    , Scheme (..)
    , HttpException (..)
    )
--import Debug.Trace (trace)

import Types
    ( Post (..)
    , PostPart (..)
    , Attachment (..)
    )
import Pageparsers
    ( threadsInCatalog
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

lainchan_domain :: String
lainchan_domain = "167.99.9.53"

lainchan_base :: Url 'Http
lainchan_base = http $ Text.pack lainchan_domain

lainchan_b :: Url 'Http
lainchan_b = lainchan_base /: "b" /: "index.html"

lainchan_post_url :: Url 'Http
lainchan_post_url = lainchan_base/: "post.php"

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


fetchAndProcessPageData
    :: Maybe String
    -> (ByteString -> IO a)
    -> Url scheme
    -> Option scheme
    -> IO (Either (Int, Maybe ByteString) (a, CookieJar))
fetchAndProcessPageData datadir process url params = do
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

fetchPostsFromBunkerCatalogPage
    :: Maybe String
    -> Url a
    -> Option a
    -> IO (Maybe [ String ])
fetchPostsFromBunkerCatalogPage datadir url params = do
    e <- fetchAndProcessPageData datadir process url params

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
    e <- fetchAndProcessPageData datadir process url params

    case e of
        Left i -> do
            putStrLn $ "ERROR getting thread page " ++ show url ++ "! status code: " ++ show i
            return Nothing
        Right x -> return $ Just $ fst x

    -- fetchAndProcessPageData process url params >>= return . fst

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
    e <- fetchAndProcessPageData Nothing process url params

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
                (header "Referer" (BS.pack $ "http://" ++ lainchan_domain ++ "/b/index.html"))
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
                                (header "Referer" (BS.pack $ "http://" ++ lainchan_domain ++ threadUrlStr))
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
