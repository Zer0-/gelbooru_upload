{-# LANGUAGE OverloadedStrings, DataKinds, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- There is an orphan instance of CookieJar which we need to not login every request
-- I don't consider this a hack, CookieJar should derive something like this already

module Main
    ( main
    ) where

import System.Environment (getArgs)
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64.URL as BSURL
import Data.Semigroup (Endo (..))
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
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
    , ReqBodyJson (..)
    , https
    , http
    , port
    , bsResponse
    , jsonResponse
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
import Data.Aeson
    ( Value
    )

import Types
    ( Post (..)
    , PostPart (..)
    , Attachment (..)
    , PostWithDeps
    , PostId
    )
import Pageparsers
    ( threadsInCatalog
    , lainchanFormParams
    -- , lainchanFirstReply
    , lainchanPostNumbers
    , postsInThread
    , flatten
    , FormField (..)
    )
import FSMemoize (fsmemoize)
import Retopo (postsDeps, indexPosts, orderDeps, mapPostQuoteLinks)

instance Serialize CookieJar where
    put c = put $ show c
    get = get >>= return . read

type PostWithAttachments scheme = (Post, [ (Url scheme, Maybe String, ByteString) ])

type HttpResponseDat = (Maybe String, ByteString, CookieJar)
type HttpResponseWithMimeAndCookie =
    Either (Int, Maybe ByteString) HttpResponseDat

type SpamNoticerResponse =
    Either (Int, Maybe ByteString) Value

printTime :: String -> POSIXTime -> IO ()
printTime name t = do
    t2 <- getPOSIXTime
    putStrLn $ "BLOCK " ++ name ++ " took " ++ show (dt t2) ++ " seconds to run"

    where
        dt :: POSIXTime -> Double
        dt t2 = (realToFrac t2) - (realToFrac t)

lainchan_domain :: String
-- lainchan_domain = "167.99.9.53"
lainchan_domain = "192.168.4.6"

lainchan_port :: Option scheme
lainchan_port = port 8080

lainchan_base :: Url 'Http
lainchan_base = http $ Text.pack lainchan_domain

{-
lainchan_b :: Url 'Http
lainchan_b = lainchan_base /: "b" /: "index.html"
-}

lainchan_post_url :: Url 'Http
lainchan_post_url = lainchan_base /: "post.php"

spam_url_base :: Url 'Http
spam_url_base = http $ Text.pack "127.0.0.1"

spam_post_url :: Url 'Http
spam_post_url = spam_url_base -- /: "/"

spam_port :: Option scheme
spam_port = port 8080

leftychan_port :: Option scheme
--leftychan_port = port 8081
leftychan_port = mempty

leftychan_root :: Url 'Https
leftychan_root = https "leftychan.net"

{-
leftychan_leftypol_catalog :: Url 'Https
leftychan_leftypol_catalog = leftychan_root /: "leftypol" /: "catalog.html"
-}

leftychan_catalog :: String -> Url 'Https
leftychan_catalog bname = leftychan_root /: Text.pack bname /: "catalog.html"

boardNameMap :: Map String String
boardNameMap = Map.fromList
    [ ("leftypol", "leftypol")
    , ("b", "b")
    , ("GET", "GET")
    , ("hobby", "hobby")
    , ("games", "games")
    , ("edu", "edu")
    , ("anime", "anime")
    , ("ref", "ref")
    , ("tech", "tech")
    , ("gulag", "gulag")
    , ("dead", "dead")
    ]

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


spamExceptionHandler :: HttpException -> IO SpamNoticerResponse
spamExceptionHandler
    ( VanillaHttpException
        ( HTTP.HttpExceptionRequest
            _ -- Request
            (HTTP.StatusCodeException resp bs)
        )
    ) = return $ Left (statusCode $ responseStatus resp, Just bs)
spamExceptionHandler _ = return $ Left (0, Nothing)


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

fetchPostsFromLeftychanCatalogPage
    :: Maybe String
    -> Url a
    -> Option a
    -> IO (Maybe [ String ])
fetchPostsFromLeftychanCatalogPage datadir url params = do
    e <- fetchAndProcessPageData datadir process url params

    case e of
        Left (i, bs) -> do
            putStrLn $ "ERROR getting catalog page " ++ show url ++ " status code " ++ show i ++ " response:"
            print bs
            return Nothing
        Right x -> return $ Just $ fst x

    where
        process rawdoc = do
            print rawdoc
            print $ renderParams params
            putStrLn "threads on this catalog page:"
            threadPaths <- threadsInCatalog rawdoc
            mapM_ putStrLn threadPaths
            putStrLn ""
            return threadPaths

fetchLeftychanchanPostsInThread :: Maybe String -> Url a -> Option a -> IO (Maybe [ Post ])
fetchLeftychanchanPostsInThread datadir url params = do
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
            putStrLn $ "posts in this thread (" ++ show url ++ "):"
            -- print rawdoc
            postss <- postsInThread rawdoc
            mapM_ print postss
            putStrLn ""
            return postss

fetchLainchanFormPage :: Url a -> Option a -> IO ([ FormField ], CookieJar)
fetchLainchanFormPage url params = do
    e <- fetchAndProcessPageData Nothing lainchanFormParams url params

    case e of
        Left (ie, bs) -> do
            putStrLn $ "GET form page " ++ show url ++ " failed! waiting and retrying."
            putStrLn $ "Status Code: " ++ show ie ++ " \n" ++ show bs
            threadDelay $ 5 * (1000 * 1000)
            fetchLainchanFormPage url params
        Right x -> return x
            

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


fetchAttachments :: String ->  Post -> IO (Maybe (PostWithAttachments 'Https))
fetchAttachments datadir post2 = do
    saved <-
        ( mapM
            ( \a ->
                let u = mkBnkrUrl $ prepareUrlStr $ attachmentUrl a
                {-
                 - TODO:
                 -
                 - The result of this "saved" will be a Maybe.
                 - This needs to be zipped with the Attachments list in posts2
                 - Then return posts2 with the  attachments list filtered
                 - on the gotten attachment value not being Nothing
                 -
                 - (basically keep the attachment metadata in post2 consistent
                 - with what we can actually fetch, but this doesn't seem
                 - to be important)
                 -}
                in
                    ( do
                        t1 <- getPOSIXTime
                        result <- getFn u
                        printTime "ATTACHMENT" t1
                        return result
                    )
                    >>=
                    ((\p ->
                        case p of
                            Left (i, response) -> do
                                putStrLn $ "[ATTACHMENT ERROR] Could not fetch attachment " ++ show u
                                    ++ " status code: " ++ show i ++ " response:\n" ++ show response
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
        getFn = (flip (cachedGetB datadir)) leftychan_port

        mkBnkrUrl = mkUrl leftychan_root

        prepareUrlStr = Text.pack . (drop 1)

mkUrl :: Url a -> Text.Text -> Url a
mkUrl root = (foldl (/:) root) . (Text.splitOn "/")

postToSpamNoticer :: PostWithAttachments scheme -> IO SpamNoticerResponse
postToSpamNoticer (post, attachments) =
    -- handle spamExceptionHandler $ runReq httpConfig $ do
    runReq httpConfig $ do
        let payload = undefined

        r <- req
            POST
            spam_post_url
            (ReqBodyJson (payload :: Value))
            jsonResponse
            spam_port

        case responseStatusCode r of
            200 -> liftIO $ return $ Right $ responseBody r
            e   -> error $ "Upload failed! result code " ++ show e


mainPostLoop :: String -> [ PostWithDeps ] -> IO ()
mainPostLoop _ [] = return ()
mainPostLoop datadir ((post2, threadId, _):ps) = do
    t1 <- getPOSIXTime
    putStrLn "Fetch attachments"
    mPwA <- fetchAttachments datadir post2
    putStrLn "Fetch attachments OK"
    let postFn = postToSpamNoticer

    case mPwA of
        Just pwA -> do
            putStrLn "calling postFn"
            ok <- postFn pwA
            case ok of
                Left (ie, bs) -> do
                        putStrLn $ "POST  failed! Status Code: " ++ show ie
                        putStrLn $ show bs
                        mainPostLoop datadir ps
                Right jsResp -> do
                    putStrLn "Have raw reply from POST!"
                    print jsResp
                    mainPostLoop datadir ps

        Nothing -> printTime sectionName t1 >> mainPostLoop datadir ps

    where
        -- for debugging only:
        sectionName = "MainPostLoop"

main :: IO ()
main = do
    args <- getArgs
    let datadir = head args
    let datadir2 = head $ drop 1 args -- for attachments separately
    let boardname = head $ drop 2 args

    putStrLn $ "datadir: " ++ datadir

    mThreadPaths <- fetchPostsFromLeftychanCatalogPage
            (Just datadir)
            --leftychan_leftypol_catalog
            (leftychan_catalog boardname)
            leftychan_port

    threads1 <- case mThreadPaths of
        Nothing -> return []
        Just threadPaths ->
            mapM
                ( \u ->
                    ((flip (fetchLeftychanchanPostsInThread (Just datadir))) leftychan_port) $
                        mkUrl leftychan_root u
                )
                (map (Text.pack . (drop 1)) $ reverse threadPaths)

    let threads = (flatten threads1) :: [[ Post ]] -- ✓

    putStrLn $ "have " ++ (show $ length threads) ++ " threads!"

    let orderedPosts = (orderDeps $ indexPosts $ postsDeps boardname threads) :: [ PostWithDeps ]

    -- mapM_ print orderedPosts

    mainPostLoop datadir2 orderedPosts

    -- putStrLn "Done"
