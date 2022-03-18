{-# LANGUAGE DataKinds, Arrows #-}

module Pageparsers
    ( threadsInCatalog
    , postsInThread
    , flatten
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
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.ISO8601 (parseISO8601)
import qualified Data.Tree.Class as Tree
import Data.Tree.NTree.TypeDefs (NTree (..), NTrees)
import Text.XML.HXT.DOM.TypeDefs (XNode (..))
import Text.XML.HXT.DOM.QualifiedName (qualifiedName)
import Data.Text (unpack)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
--import Text.XML.HXT.DOM.ShowXml (xshow)
import Text.XML.HXT.CSS (css)
import Control.Arrow.IOStateListArrow (IOSLA)
import Text.XML.HXT.Arrow.XmlState.TypeDefs (XIOState)
import Text.XML.HXT.DOM.TypeDefs (XmlTree)

import Types (Attachment (..), PostPart (..), Post (..))

flatten :: [ Maybe a ] -> [ a ]
flatten = (=<<) (maybe [] (: []))

type Doc a = IOSLA (XIOState ()) XmlTree a

mkdoc :: ByteString -> Doc XmlTree
mkdoc
    = (readString [ withParseHTML yes, withWarnings no])
    . (unpack . decodeUtf8)


threadsInCatalog :: ByteString -> IO [ String ]
threadsInCatalog rawdoc =
    --runX $ mkdoc rawdoc >>> css "#divThreads > .catalogCell > .linkThumb" >>> getAttrValue "href"
    runX $ mkdoc rawdoc >>> css "#Grid .thread > a" >>> getAttrValue "href"


parseTime :: String -> UTCTime
parseTime = fromMaybe (posixSecondsToUTCTime $ 0) . parseISO8601

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
    | qualifiedName qn == "a" && elem "_blank" (xGetAttr "target" xs)
        = PostedUrl $ head (xGetAttr "href" xs)
    | qualifiedName qn == "a"
        = Quote $ head (xGetText c)
    | qualifiedName qn == "a" = PostedUrl $ head (xGetText c)
    | qualifiedName qn == "span" && elem "quote" (xGetAttr "class" xs)
        = GreenText $ map postPartFromXmlTree c
    | qualifiedName qn == "span" && elem "orangeQuote" (xGetAttr "class" xs)
        = OrangeText $ map postPartFromXmlTree c
    | qualifiedName qn == "span" && elem "heading" (xGetAttr "class" xs)
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

parseOP :: Doc Post
parseOP =
    proc l -> do
        opSubjectField <- withDefault getOpSubject Nothing -< l
        nameField <- getName -< l
        emailField <- withDefault getEmail Nothing -< l
        postNumberField <- css ".post.op .post_no:not([id])" >>> getChildren >>> getText -< l
        postParts <- withDefault getBody [] -< l
        attachmentUrls <- withDefault (listA getAttachments) [] -< l
        timeField <- css ".post.op time" >>> getAttrValue "datetime" -< l

        returnA -< Post
            { attachments = map (\(f, (u, t)) -> Attachment f u t) attachmentUrls
            , subject = opSubjectField
            , email = emailField
            , name = nameField
            , postNumber = read postNumberField
            , postBody = postParts
            , timestamp = parseTime timeField
            }

    where
        getOpSubject = css ".post.op .subject" >>> getChildren >>> getText >>> arr Just
        getName = css ".post.op .name" >>> getChildren >>> getText
        getBody = css ".post.op .body" >>> parseBody
        getEmail
            = css ".post.op .email"
            >>> hasAttr "href"
            >>> getAttrValue "href"
            >>> arr (Just . extractEmail)

        extractEmail = drop (length "mailto:")

        getAttachments
            = css ".post_anchor ~ .files .file"
            >>> ( (css ".fileinfo .details .postfilename" >>> getChildren >>> getText)
                &&& (css ".fileinfo > a" >>> getAttrValue "href")
                &&& (withDefault (css ".post-image" >>> getAttrValue "src" >>> arr Just) Nothing)
                )

parsePost :: Doc Post
parsePost =
    proc l -> do
        attachmentUrls <- withDefault (listA getAttachments) [] -< l
        subjectField <- withDefault getSubject Nothing -< l
        nameField <- getName -< l
        emailField <- withDefault getEmail Nothing -< l
        postNumberField <- css ".post_no:not([id])" >>> getChildren >>> getText -< l
        timeField <- css "time" >>> getAttrValue "datetime" -< l

        postParts <- withDefault getBody [] -< l

        returnA -< Post
            { attachments = map (\(f, (u, t)) -> Attachment f u t) attachmentUrls
            , subject = subjectField
            , email = emailField
            , name = nameField
            , postNumber = read postNumberField
            , postBody = postParts
            , timestamp = parseTime timeField
            }

    where
        getSubject = css ".subject" >>> getChildren >>> getText >>> arr Just
        getName = css ".name" >>> getChildren >>> getText

        getEmail
            = css ".email"
            >>> hasAttr "href"
            >>> getAttrValue "href"
            >>> arr (Just . extractEmail)

        extractEmail = drop (length "mailto:")

        getBody = css ".body" >>> parseBody

        getAttachments
            = css ".file"
            >>> ( (css ".fileinfo .details .postfilename" >>> getChildren >>> getText)
                &&& (css ".fileinfo > a" >>> getAttrValue "href")
                &&& (withDefault (css ".post-image" >>> getAttrValue "src" >>> arr Just) Nothing)
                )


postsInThread :: ByteString -> IO [ Post ]
postsInThread rawdoc =
    (runX $
        mkdoc rawdoc >>> proc l ->
                do
                    op <- css ".thread" >>> parseOP -< l
                    ps <- listA getPosts -< l
                    returnA -< op : ps

    )
    >>= return . head

    where
        getPosts = css ".post.reply" >>> parsePost
