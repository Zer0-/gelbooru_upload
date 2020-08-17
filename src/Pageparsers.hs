{-# LANGUAGE DataKinds, Arrows #-}

module Pageparsers
    ( imageLinks
    , imagePageFilenameTags
    , posts
    , threadsInCatalog
    , postsInThread
    , Post
    ) where

import Text.XML.HXT.Core
    ( readString
    , withParseHTML
    , withWarnings
    , runX
    , yes
    , no
    , (>>>)
    , getAttrValue
    , getText
    , getChildren
    , hasAttr
    , returnA
    , withDefault
    , arr
    , listA
    )
import Data.Text (unpack, pack)
import Text.URI (mkURI, URI)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
import Data.List.Split (splitOn)
--import Text.XML.HXT.DOM.ShowXml (xshow)
import Text.XML.HXT.CSS (css)
import Network.HTTP.Req (Option, useHttpsURI, Scheme (..))
import Control.Arrow.IOStateListArrow (IOSLA)
import Text.XML.HXT.Arrow.XmlState.TypeDefs (XIOState)
import Text.XML.HXT.DOM.TypeDefs (XmlTree)

flatten :: [ Maybe a ] -> [ a ]
flatten = (=<<) (maybe [] (: []))

parseURIs :: [ String ] -> [ URI ]
parseURIs = flatten . (map f)
    where
        f = mkURI . pack . ((++) "https://")


type Doc a = IOSLA (XIOState ()) XmlTree a

data Attachment = Attachment
    { filename :: String
    , url :: String
    } deriving Show

data PostPart
    = SimpleText String
    | PostedUrl String
    | Skip
    | Quote String
        -- Quotes don't seem to be able to be spoilered
        -- board links (which appear as quotes but start with >>>) break the tag
    | GreenText String
    | OrangeText String
    | RedText String
    | Spoiler PostPart
    -- you can't seem to spoiler greentext
    | Bold String
    | Underlined String
    | Italics String
    | Strikethrough String
    deriving Show

data Post = Post
    { attachments :: [ Attachment ]
    , subject :: Maybe String
    , email :: Maybe String
    , name :: String
    , post :: [ PostPart ]
    } deriving Show

--data Post = Post { subject :: Maybe String } deriving Show

mkdoc :: ByteString -> Doc XmlTree
mkdoc
    = (readString [ withParseHTML yes, withWarnings no])
    . (unpack . decodeUtf8)

imageLinks :: ByteString -> IO [ Option 'Https ]
imageLinks rawdoc = do
    elems <- runX $ mkdoc rawdoc >>> css "span.thumb > a" >>> getAttrValue "href"

    return $ map snd $ flatten $
        map useHttpsURI (parseURIs elems)


imagePageFilenameTags :: ByteString -> IO (String, [ String ])
imagePageFilenameTags rawdoc = do
    fname <- runX $ mkdoc rawdoc >>> css "#image" >>> getAttrValue "src"
    tags <- runX $ mkdoc rawdoc >>> css "#tag_list > ul > li > span > a" >>> getChildren >>> getText

    return (head fname, tags)

{- Bunkerchan -}
threadsInCatalog :: ByteString -> IO [ String ]
threadsInCatalog rawdoc =
    runX $ mkdoc rawdoc >>> css "#divThreads > .catalogCell > .linkThumb" >>> getAttrValue "href"


{- Bunkerchan -}
parsePost :: Doc Post
parsePost =
    proc l -> do
        subjectField <- withDefault getSubject Nothing -< l
        nameField <- getName -< l
        emailField <- withDefault getEmail Nothing -< l

        returnA -< Post
            { attachments = []
            , subject = subjectField
            , email = emailField
            , name = nameField
            , post = []
            }

    where
        getSubject = css ".labelSubject" >>> getChildren >>> getText >>> arr Just
        getName = css "a.linkName" >>> getChildren >>> getText
        getEmail
            = css "a.linkName"
            >>> hasAttr "href"
            >>> getAttrValue "href"
            >>> arr (Just . extractEmail)
        extractEmail = drop (length "mailto:")

--getChildren >>> getText

{- Bunkerchan -}
postsInThread :: ByteString -> IO [ Post ]
postsInThread rawdoc = do
    allPosts <- runX $
        mkdoc rawdoc >>> proc l ->
                do
                    op <- css ".opHead" >>> parsePost -< l
                    ps <- listA getPosts -< l
                    returnA -< (op, ps)

    return $ (\(op, xs) -> op : xs) (head allPosts)

    where
        getPosts = css ".postCell > div" >>> parsePost

posts :: ByteString -> IO [ (Int, String) ]
posts rawdoc = do
    ids <- runX $ doc >>> css "span.thumb > a" >>> getAttrValue "id"
    filenames <- runX $ doc >>> css "span.thumb > a > img" >>> getAttrValue "src"

    return $ zip (map parseId ids) (map parseFilename filenames)

    where
        doc = mkdoc rawdoc

        parseId :: String -> Int
        parseId = read . (drop 1)

        parseFilename :: String -> String
        parseFilename = last . (splitOn "_")
