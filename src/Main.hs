{-# LANGUAGE OverloadedStrings, DataKinds, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- There is an orphan instance of CookieJar which we need to not login every request
-- I don't consider this a hack, CookieJar should derive something like this already

module Main
    ( main
    ) where

import Prelude hiding (writeFile)
import System.Environment (getArgs)
import Data.ByteString (ByteString, writeFile)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64.URL as BSURL
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime, diffTimeToPicoseconds, diffUTCTime)
import Control.Monad.IO.Class (liftIO)
import Control.Exception.Safe (handle)
import Data.Serialize (Serialize (..))
import qualified Crypto.Hash.MD5 as MD5
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client
    ( CookieJar
    , responseStatus
    )
import Network.HTTP.Types (renderQuery, queryTextToQuery)
import Network.HTTP.Types.Status (Status (statusCode))

import Network.HTTP.Req
    ( runReq
    , req
    , defaultHttpConfig
    , HttpConfig (..)
    , GET (..)
    , POST (..)
    , NoReqBody (..)
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
    , (/:)
    , Url
    , Scheme (..)
    , Option
    , HttpException (..)
    , renderUrl
    , queryParamToList
    )
import Data.Aeson
    ( Value
    , object
    , (.=)
    )

import Types
    ( Post (..)
    , PostPart (..)
    , Attachment (..)
    , PostWithDeps
    )
import Pageparsers
    ( threadsInCatalog
    , postsInThread
    , flatten
    )
import FSMemoize (fsmemoize)
import Retopo (postsDeps, indexPosts, orderDeps)

instance Serialize CookieJar where
    put c = put $ show c
    get = get >>= return . read

type PostWithAttachments scheme = (Post, [ (Attachment, (Url scheme, Maybe String, ByteString)) ])

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

spam_url_base :: Url 'Http
spam_url_base = http $ Text.pack "127.0.0.1"

spam_post_url :: Url 'Http
spam_post_url = spam_url_base -- /: "/"

spam_port :: Option scheme
spam_port = port 8080

leftychan_port :: Option scheme
--leftychan_port = port 8081
leftychan_port = mempty

website :: String
website = "leftychan.net"

leftychan_root :: Url 'Https
leftychan_root = https "leftychan.net"

{-
leftychan_leftypol_catalog :: Url 'Https
leftychan_leftypol_catalog = leftychan_root /: "leftypol" /: "catalog.html"
-}

leftychan_catalog :: String -> Url 'Https
leftychan_catalog bname = leftychan_root /: Text.pack bname /: "catalog.html"

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
renderParams params = renderQuery True $ queryTextToQuery $ queryParamToList params

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


copyThreadInfo :: String -> Post -> Post -> Post
copyThreadInfo board a b =
    b
        { websiteName = website
        , boardName = board
        , threadId = threadId a
        }


fixThreadIds :: String -> [ Post ] -> [ Post ]
fixThreadIds _ [] = []
fixThreadIds board (op : []) = (copyThreadInfo board op op) : []
fixThreadIds board (op : x : xs) = (copyThreadInfo board op op) : (fixThreadIds board $ (copyThreadInfo board op x) : xs)


fetchLeftychanchanPostsInThread :: Maybe String -> String -> Url a -> Option a -> IO (Maybe [ Post ])
fetchLeftychanchanPostsInThread datadir board url params = do
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
            return $ fixThreadIds board postss

renderPostBody :: [ PostPart ] -> String
renderPostBody = ((=<<) :: (PostPart -> String) -> [ PostPart ] ->  String) renderPart
    where
        renderPart :: PostPart -> String
        renderPart (SimpleText s) = s
        renderPart (PostedUrl s) = s
        renderPart Skip = "\n"
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

fetchAttachments :: String ->  Post -> IO (Maybe (PostWithAttachments 'Https))
fetchAttachments datadir post2 = do
    saved <-
        ( mapM
            ( \a ->
                let u = (mkUrl leftychan_root) $ prepareUrlStr $ attachmentUrl a
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
                            Right (m, b, _) -> return $ Just (a, (u, m, b))
                    ) :: HttpResponseWithMimeAndCookie -> IO (Maybe (Attachment, (Url 'Https, Maybe String, ByteString))))
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

        prepareUrlStr = Text.pack . (drop 1)

mkUrl :: Url a -> Text.Text -> Url a
mkUrl root = (foldl (/:) root) . (Text.splitOn "/")

utcTimeToSeconds :: UTCTime -> Integer
utcTimeToSeconds t = 
    diffTimeToPicoseconds (realToFrac (diffUTCTime t epoch))
    `div`
    (1000000 * 1000 * 1000)

    where
        epoch = posixSecondsToUTCTime 0

postToSpamNoticer
    :: String
    -> PostWithAttachments scheme
    -> IO SpamNoticerResponse
postToSpamNoticer datadir (post, attachments) = do
    attachment_objs <- mapM attachmentObj attachments

    let payload = object
            [ "time_stamp" .= (utcTimeToSeconds $ timestamp post)
            , "body" .= (renderPostBody $ postBody post)
            , "attachments" .= attachment_objs
            , "website_name" .= websiteName post
            , "board_name" .= boardName post
            , "thread_id" .= threadId post
            ]

    putStrLn "PAYLOAD:"
    print payload

    handle spamExceptionHandler $ runReq httpConfig $ do

        r <- req
            POST
            spam_post_url
            (ReqBodyJson (payload :: Value))
            jsonResponse
            spam_port

        case responseStatusCode r of
            200 -> liftIO $ return $ Right $ responseBody r
            e   -> error $ "Upload failed! result code " ++ show e

    where
        attachmentObj :: (Attachment, (Url scheme, Maybe String, ByteString)) -> IO Value
        attachmentObj (a, (_, maybe_mime, bs)) = do
            let md5sum = encodeHex $ MD5.hash bs

            let filename = datadir ++ "/" ++ Text.unpack md5sum ++ ".attachment"

            writeFile filename bs

            return $ object
                [ "mimetype" .= (mimetype maybe_mime)
                , "md5_hash" .= md5sum
                , "filename" .= filename
                , "thumbnail_url" .= (renderUrl $ mkUrl leftychan_root $ Text.pack $ thumbnailUrl a)
                ]

        mimetype :: Maybe String -> String
        mimetype (Just m) = m
        mimetype Nothing = "application/octet-stream"

        
encodeHex :: ByteString -> Text.Text
encodeHex bs =
    Text.decodeUtf8 (Base16.encode bs)


mainPostLoop :: String -> [ PostWithDeps ] -> IO ()
mainPostLoop _ [] = return ()
mainPostLoop datadir ((post2, _ {- thread id -}, _):ps) = do
    t1 <- getPOSIXTime
    putStrLn "Fetch attachments"
    mPwA <- fetchAttachments datadir post2
    putStrLn "Fetch attachments OK"
    let postFn = postToSpamNoticer datadir

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
                    ((flip (fetchLeftychanchanPostsInThread (Just datadir) boardname)) leftychan_port) $
                        mkUrl leftychan_root u
                )
                (map (Text.pack . (drop 1)) $ reverse threadPaths)

    let threads = (flatten threads1) :: [[ Post ]] -- ✓

    putStrLn $ "have " ++ (show $ length threads) ++ " threads!"

    let orderedPosts = (orderDeps $ indexPosts $ postsDeps boardname threads) :: [ PostWithDeps ]

    -- mapM_ print orderedPosts

    mainPostLoop datadir2 orderedPosts

    putStrLn "Done"


{-
 -
 - Since uploading leftypol took 9h (which averages about .8 posts per second, which is actually a decent rate)
 - we can truncate the size of the "recent text posts" table, and try to get this to run in under
 - a certain time?
 -
 - why the time constraints?
 -      - it keeps data safe. Board data needs to be able to move freely, and if it has to pass through
 -      the spamfilter it shouldn't be hindered. Though in that case, fuck the filter.
 -      - what is fast enough?
 - need to fix 500 error found in /tmp/asdf.txt
 -      - what are we posting that results in a 500 error from spamnoticer?
 -      - did the optional argument we passed to web.Application help?
 -
 - need to get rid of false positives
 -      - simply turn down the rate for text posts
 - need to investigate if any of the "potential" negatives (things that had 6
 - occurrences) need to be noticed, and if so, the rate function should be
 - tweaked.
 -      - plot some time series of similar posts to see if this is a problem,
 -      it could help to visualize it
 - need to write a UI
 - need to integrate with the board software via REST
 -}
