{-# LANGUAGE DataKinds #-}

module Pageparsers
    ( imageLinks
    , imagePageFilenameTags
    , posts
    , threadsInCatalog
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


type Doc = IOSLA (XIOState ()) XmlTree XmlTree

mkdoc :: ByteString -> Doc
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
    filename <- runX $ mkdoc rawdoc >>> css "#image" >>> getAttrValue "src"
    tags <- runX $ mkdoc rawdoc >>> css "#tag_list > ul > li > span > a" >>> getChildren >>> getText

    return (head filename, tags)

threadsInCatalog :: ByteString -> IO [ String ]
threadsInCatalog rawdoc =
    runX $ mkdoc rawdoc >>> css "#divThreads > .catalogCell > .linkThumb" >>> getAttrValue "href"

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
