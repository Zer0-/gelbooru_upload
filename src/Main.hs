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

lainchan_domain :: String
lainchan_domain = "167.99.9.53"

lainchan_base :: Url 'Http
lainchan_base = http $ Text.pack lainchan_domain

{-
lainchan_b :: Url 'Http
lainchan_b = lainchan_base /: "b" /: "index.html"
-}

lainchan_post_url :: Url 'Http
lainchan_post_url = lainchan_base /: "post.php"

bunkerchan_port :: Option scheme
--bunkerchan_port = port 8080
bunkerchan_port = mempty

-- bunkerchan_root :: Url 'Http
-- bunkerchan_root = http "192.168.4.6" -- "127.0.0.1"

bunkerchan_root :: Url 'Https
bunkerchan_root = https "bunkerchan.xyz"

bunkerchan_leftypol_catalog :: Url 'Https
bunkerchan_leftypol_catalog = bunkerchan_root /: "leftypol" /: "catalog.html"

boardNameMap :: Map String String
boardNameMap = Map.fromList [ ("leftypol", "b") ]

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
    e <- fetchAndProcessPageData Nothing lainchanFormParams url params

    case e of
        Left (ie, bs) -> do
            putStrLn $ "GET form page " ++ show url ++ " failed! waiting and retrying."
            putStrLn $ "Status Code: " ++ show ie ++ " \n" ++ show bs
            threadDelay $ 5 * (1000 * 1000)
            fetchLainchanFormPage url params
        Right x -> return x
            

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


{-
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

    -- what is ss? form field list aparantly. [ FormField ]
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
-}


mkUrl :: Url a -> Text.Text -> Url a
mkUrl root = (foldl (/:) root) . (Text.splitOn "/")

postOnLainchan
    :: Bool
    -> PostId
    -> PostWithAttachments schema
    -> IO HttpResponseWithMimeAndCookie
postOnLainchan isOP (boardname, threadId) (post2, attch) = do
    putStrLn $ "fetching form page " ++ referrerUrl
    (ss, _) <- fetchLainchanFormPage formPageUrl mempty

    putStrLn "Posting:"
    putStrLn $ "isOP: " ++ show isOP
    putStrLn $ renderPostBody $ postBody post2
    print (postNumber post2, subject post2, attachments post2)

    postLainchan
        lainchan_post_url
        (header "Referer" (BS.pack $ referrerUrl))
        (post2, attch)
        ss

    where
        formPageUrl = mkUrl lainchan_base $ Text.pack formPagePath

        formPagePath =
            if isOP
            then newboardname ++ "/index.html"
            else boardname ++ "/res/" ++ show threadId ++ ".html"

        referrerUrl = "http://" ++ lainchan_domain ++ "/" ++ formPagePath

        newboardname = boardNameMap Map.! boardname


mainPostLoop :: String -> Map PostId PostId -> [ PostWithDeps ] -> IO ()
mainPostLoop _ _ [] = return ()
mainPostLoop datadir pMap ((post2, threadId, _):ps) = do
    putStrLn " Fetch attachments"
    mPwA <- fetchAttachments datadir post2
    putStrLn " Fetch attachments OK"
    let isOP = Map.notMember threadId pMap
    let postFn = postOnLainchan isOP (if isOP then threadId else (pMap Map.! threadId))

    case mPwA of
        Just pwA -> do
            putStrLn "calling postFn"
            ok <- postFn (fixPost pwA)
            case ok of
                Left (ie, bs) -> do
                        putStrLn $ "POST  failed! Status Code: " ++ show ie
                        putStrLn $ show bs
                        mainPostLoop datadir pMap ps
                Right (_, rawreply, _) -> do
                    putStrLn "Have raw reply from POST!"
                    threadPosts <- lainchanPostNumbers rawreply

                    let newpostnum =
                            read $ (if isOP then head else last) threadPosts :: Int
                        newpMap = Map.insert
                            (boardname, postNumber post2)
                            (newboardname, newpostnum)
                            pMap
                        newpMap2 =
                            if isOP then
                                Map.insert
                                    threadId
                                    (newboardname, newpostnum)
                                    newpMap
                            else newpMap
                        in do
                                putStrLn $ "old post num: " ++ show (postNumber post2)
                                putStrLn $ "new post num: " ++ show newpostnum
                                putStrLn "mainPostLoop looping"
                                mainPostLoop datadir newpMap2 ps
        Nothing -> mainPostLoop datadir pMap ps -- this means that we don't modify pMap

    where
        fixPost :: PostWithAttachments scheme -> PostWithAttachments scheme
        fixPost (p, a) = (mapPostQuoteLinks boardname newboardname pMap p, a)

        boardname = fst threadId

        newboardname = boardNameMap Map.! boardname

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

    putStrLn $ "have " ++ (show $ length threads) ++ " threads!"

    let orderedPosts = (orderDeps $ indexPosts $ postsDeps "leftypol" threads) :: [ PostWithDeps ]

    -- mapM_ print orderedPosts

    mainPostLoop datadir Map.empty orderedPosts

    {-
     - create mapP :: Map PostId PostId    -- (old post id -> new post id)
     -
     - mainPostLoop :: mapP -> [ post ] -> IO ()
     -
     - For each (post, thread) in orderedPosts:
     -      - post is OP iff thread not in mapP
     -
     -      - set up postFn based on isOP, boardname, threadId
     -
     -      - fetch post attachments to get mPwA :: Maybe PostWithAttachments
     -          case mPwA of
     -              Nothing -> mainPostLoop mapP nextStuff 
     -              Just pwA -> do
     -                  ok <- postFn (fixPostQuoteLinks pwA mapP)
     -                  case ok of
     -                      Left (id, bs) -> do
     -                          -- print error
     -                          mainPostLoop mapP nextStuff 
     -                      Right (_, rawreply, _) ->
     -                          -- get the new post number
     -                          -- add entry to mapP
     -                          -- keep posting!
     -}

    putStrLn "Done"
