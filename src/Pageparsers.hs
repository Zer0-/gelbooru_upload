{-# LANGUAGE DataKinds #-}

module Pageparsers
    ( imageLinks
    , imagePageFilenameTags
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
    , processChildren
    , getChildren
    , isText
    )
import Data.Text (unpack, pack)
import Text.URI (mkURI, URI)
import Data.ByteString (ByteString)
import Data.Text.Encoding (decodeUtf8)
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
