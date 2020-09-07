{-# LANGUAGE DataKinds, Arrows #-}

module Pageparsers
    ( imageLinks
    , imagePageFilenameTags
    , posts
    , threadsInCatalog
    , lainchanFormParams
    , lainchanFirstReply
    , postsInThread
    , flatten
    , FormField (..)
    ) where

import Text.XML.HXT.Core
    ( readString
    , withParseHTML
    , withWarnings
    , runX
    , yes
    , no
    , (>>>)
    , (&&&)
    , getAttrValue
    , getText
    , getChildren
    , hasAttr
    , returnA
    , withDefault
    , arr
    , listA
    )
import qualified Data.Tree.Class as Tree
import Data.Tree.NTree.TypeDefs (NTree (..), NTrees)
import Text.XML.HXT.DOM.TypeDefs (XNode (..))
import Text.XML.HXT.DOM.QualifiedName (qualifiedName)
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

import Types (Attachment (..), PostPart (..), Post (..))

flatten :: [ Maybe a ] -> [ a ]
flatten = (=<<) (maybe [] (: []))

parseURIs :: [ String ] -> [ URI ]
parseURIs = flatten . (map f)
    where
        f = mkURI . pack . ((++) "https://")


type Doc a = IOSLA (XIOState ()) XmlTree a

data FormField = FormField
    { fieldName :: String
    , fieldValue :: String
    } deriving Show

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

lainchanFormParams :: ByteString -> IO [ FormField ]
lainchanFormParams rawdoc = do
    as <- runX $ mkdoc rawdoc
        >>> css "form[name=post] input"
        >>> parseFormField

    bs <- runX $ mkdoc rawdoc
        >>> css "form[name=post] textarea"
        >>> parseTextAreaFormField

    return $ as ++ bs

lainchanFirstReply :: ByteString -> IO String
lainchanFirstReply rawdoc = do
    as <- runX $ mkdoc rawdoc
        >>> css ".thread .intro a:not([class])" >>> getAttrValue "href"

    return $ head as

parseFormField :: Doc FormField
parseFormField =
    proc l -> do
        nameField <- getAttrValue "name" -< l
        valueField <- getAttrValue "value" -< l

        returnA -< FormField
            { fieldName = nameField
            , fieldValue = valueField
            }

parseTextAreaFormField :: Doc FormField
parseTextAreaFormField =
    proc l -> do
        nameField <- getAttrValue "name" -< l
        valueField <- withDefault (getChildren >>> getText) "" -< l

        returnA -< FormField
            { fieldName = nameField
            , fieldValue = valueField
            }

{- Bunkerchan -}
threadsInCatalog :: ByteString -> IO [ String ]
threadsInCatalog rawdoc =
    runX $ mkdoc rawdoc >>> css "#divThreads > .catalogCell > .linkThumb" >>> getAttrValue "href"


         -- XmlTrees
xGetText :: NTrees XNode -> [ String ]
xGetText = flatten . (map asdf)
    where
        asdf (NTree (XText s) _) = Just s
        asdf _ = Nothing

xGetAttr :: String -> NTrees XNode -> [ String ]
xGetAttr s = foldl asdf []
    where
        asdf l (NTree (XAttr qn) xs) =
            if qualifiedName qn == s
            then l ++ (xGetText xs)
            else l
        asdf l _ = l

postPartFromXmlTree :: XmlTree -> PostPart
postPartFromXmlTree (NTree (XText s) _) = SimpleText s
postPartFromXmlTree (NTree (XTag qn xs) c)
    | qualifiedName qn == "a" && elem "quoteLink" (xGetAttr "class" xs)
        = Quote $ head (xGetText c)
    | qualifiedName qn == "a" && elem "_blank" (xGetAttr "target" xs)
        = PostedUrl $ head (xGetAttr "href" xs)
    | qualifiedName qn == "a" = Quote $ head (xGetText c)
    | qualifiedName qn == "span" && elem "greenText" (xGetAttr "class" xs)
        = GreenText $ map postPartFromXmlTree c
    | qualifiedName qn == "span" && elem "orangeText" (xGetAttr "class" xs)
        = OrangeText $ map postPartFromXmlTree c
    | qualifiedName qn == "span" && elem "redText" (xGetAttr "class" xs)
        = RedText $ map postPartFromXmlTree c
    | qualifiedName qn == "span" && elem "spoiler" (xGetAttr "class" xs)
        = Spoiler $ map postPartFromXmlTree c
    | qualifiedName qn == "em" = Italics $ map postPartFromXmlTree c
    | qualifiedName qn == "strong" = Bold $ map postPartFromXmlTree c
    | qualifiedName qn == "u" = Underlined $ map postPartFromXmlTree c
    | qualifiedName qn == "s" = Strikethrough $ map postPartFromXmlTree c
    | otherwise = Skip
--postPartFromXmlTree (NTree (XTag qn xs) c) = Quote $ (qualifiedName qn) ++ "|" ++ show xs ++ "|" ++ head (xGetText c)
postPartFromXmlTree _ = Skip

postPartsFromXmlTree :: XmlTree -> [ PostPart ]
postPartsFromXmlTree t = map postPartFromXmlTree (Tree.getChildren t)

parseBody :: Doc [ PostPart ]
parseBody = arr postPartsFromXmlTree


{- Bunkerchan -}
parsePost :: Doc Post
parsePost =
    proc l -> do
        subjectField <- withDefault getSubject Nothing -< l
        nameField <- getName -< l
        emailField <- withDefault getEmail Nothing -< l
        postNumberField <- css "a.linkQuote" >>> getChildren >>> getText -< l
        postParts <- withDefault getBody [] -< l
        attachmentUrls <- withDefault (listA getAttachments) [] -< l

        returnA -< Post
            { attachments = map (\(f, u) -> Attachment f u) attachmentUrls
            , subject = subjectField
            , email = emailField
            , name = nameField
            , postNumber = read postNumberField
            , postBody = postParts
            }

    where
        getSubject = css ".labelSubject" >>> getChildren >>> getText >>> arr Just
        getName = css "a.linkName" >>> getChildren >>> getText

        getEmail
            = css "a.linkName"
            >>> hasAttr "href"
            >>> getAttrValue "href"
            >>> arr (Just . extractEmail)

        getBody = css ".divMessage" >>> parseBody

        extractEmail = drop (length "mailto:")

        getAttachments
            = css ".originalNameLink"
            >>> ( ( getChildren >>> getText )
                &&& (getAttrValue "href")
                )

--getChildren >>> getText

{- Bunkerchan -}
postsInThread :: ByteString -> IO [ Post ]
postsInThread rawdoc =
    (runX $
        mkdoc rawdoc >>> proc l ->
                do
                    op <- css ".innerOP" >>> parsePost -< l
                    ps <- listA getPosts -< l
                    returnA -< op : ps

    )
    >>= return . head

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
