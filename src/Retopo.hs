module Retopo
    ( indexPosts
    , postsDeps
    , orderDeps
    , mapPostQuoteLinks
    ) where

import Data.Map (Map, fromList, toList)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl')
import Text.Parsec.Pos (incSourceColumn)
import Text.Parsec.Prim (Parsec, tokenPrim, (<|>), runP)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Combinator (between, many1)

import Types
    ( PostWithDeps
    , Post (..)
    , PostPart (Quote)
    , PostId
    )

import Pageparsers (flatten)

-- Parser For Quotes Begin
--
-- >>12345
-- or
-- >>/leftypol/12345
-- Parser for these types of quote links

type PostIdA = (Maybe String, Int)

type Parser a = Parsec String () a

quoteParser :: Parser PostIdA
quoteParser = do
    isChar '>'
    isChar '>'

    onlyDigits <|> boardLink

    where
        slash = isChar '/'

        onlyDigits = do
            num <- many1 digit
            return (Nothing, read num)

        boardLink = do
            isChar '>'
            boardname <- between slash slash (many1 $ notChar '/')
            num <- many1 digit
            return (Just boardname, read num)

validDigits :: String
validDigits = "0123456789"

isChar :: Char -> Parser ()
isChar x = satisfy (\v -> if x == v then Just () else Nothing)

notChar :: Char -> Parser Char
notChar x = satisfy (\v -> if x /= v then Just v else Nothing)

digit :: Parser Char
digit = satisfy (\v -> if (elem v validDigits) then Just v else Nothing)

satisfy :: (Char -> Maybe a) -> Parser a
satisfy test = tokenPrim show updatePos test
    where
        updatePos pos _ _ = incSourceColumn pos 1

parseQuoteLink1 :: String -> Either ParseError PostIdA
parseQuoteLink1 x = runP quoteParser () "Quote" x
-- Parser End

parseQuoteLink :: String -> String -> Either ParseError PostId
parseQuoteLink boardname x =
    let result = parseQuoteLink1 x
    in
        case result of
            Left e -> Left e
            Right (Nothing, i) -> Right (boardname, i)
            Right (Just b, i) -> Right (b, i)


postDependencies :: String -> Post -> [ PostId ]
postDependencies boardname post =
    flatten $ map
        (eitherToMaybe . (parseQuoteLink boardname) . getQuoteStr)
        -- this is wrong, a postpart can be nested
        (filter isQuote $ postBody post)


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x


getQuoteStr :: PostPart -> String
getQuoteStr (Quote s) = s
getQuoteStr _ = undefined


isQuote :: PostPart -> Bool
isQuote (Quote _) = True
isQuote _ = False


postsDeps :: String -> [[ Post ]] -> [ PostWithDeps ]
postsDeps boardname posts = posts >>= procThread
    where
        procThread :: [ Post ] -> [ PostWithDeps ]
        procThread [] = []
        procThread (x:xs) =
            let postId = (boardname, postNumber x)
            in
                -- this first post is the OP. it's id is the threadId
                -- The op only has dependencies we can gather from it's body,
                -- the rest of the posts have an implicit dependency on the
                -- post above theirs, even if they don't quote anything
                (x, postId, getFilteredDeps x)
                : procThread1 postId postId xs

        procThread1 :: PostId -> PostId -> [ Post ] -> [ PostWithDeps ]
        procThread1 _ _ [] = []
        procThread1 threadId prevPost (x:xs) =
            (x, threadId, prevPost : getFilteredDeps x)
            : procThread1 threadId (boardname, postNumber x) xs

        getFilteredDeps :: Post -> [ PostId ]
        getFilteredDeps p = filter (\(_, i) -> i < postNumber p) $
            postDependencies boardname p


indexPosts :: [ PostWithDeps ] -> Map PostId PostWithDeps
indexPosts = fromList . (map createPair)
    where
        createPair :: PostWithDeps -> (PostId, PostWithDeps)
        createPair (p, (boardname, threadid), deps) =
            ((boardname, postNumber p), (p, (boardname, threadid), deps))

orderDeps :: Map PostId PostWithDeps -> [ PostWithDeps ]
orderDeps postsMap = snd $ foldl' foldfn (Set.empty, []) (toList postsMap)
    where
        foldfn
            :: (Set.Set PostId, [ PostWithDeps ])
            -> (PostId, PostWithDeps)
            -> (Set.Set PostId, [ PostWithDeps ])
        foldfn (visited, result) (pstId, (p, thrdId, deps))
            | Set.member pstId visited = (visited, result)
            | otherwise =
                let (visited2, result2) =
                        -- recurse (foldl' again over the deps of the current post)
                        foldl'
                            foldfn
                            (Set.insert pstId visited, result)
                            (fetchMapDeps deps)
                in
                    ( visited2
                    , result2 ++ [(p, thrdId, deps)]
                    )

        fetchMapDeps = flatten . map (\p -> Map.lookup p postsMap >>= return . ((,) p))

-- TODO: go deeper into PostParts since the data structure is recursive
mapPostQuoteLinks :: String -> String -> Map PostId PostId -> Post -> Post
mapPostQuoteLinks boardname newboardname pMap post =
    post { postBody = map f (postBody post) }

    where
        -- TODO: wtf pattern match on p here for cleaner code!
        f p =
            if isQuote p
            then
                case getq p of
                    Nothing -> p
                    Just x ->
                        let newPostId = Map.findWithDefault x x pMap
                        in Quote $ renderPostIdAsQuote newPostId
            else
                p

        getq = eitherToMaybe . (parseQuoteLink boardname) . getQuoteStr

        renderPostIdAsQuote :: PostId -> String
        renderPostIdAsQuote (qbname, i)
            | newboardname == qbname = ">>" ++ show i
            | otherwise = ">>>/" ++ qbname ++ "/" ++ show i

