module Retopo
    ( indexPosts
    , parseQuoteLink
    ) where

import Data.List (foldl')
import Data.Map (Map, empty)
import Text.Parsec.Pos (incSourceColumn)
import Text.Parsec.Prim (Parsec, tokenPrim, (<|>), runP)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Combinator (between, many1)

import Types
    ( PostWithDeps
    , Post (..)
    )

-- Parser For Quotes Begin
--
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

type PostId = (String, Int) -- String is the board name

parseQuoteLink :: String -> String -> Either ParseError PostId
parseQuoteLink boardname x =
    let result = parseQuoteLink1 x
    in
        case result of
            Left e -> Left e
            Right (Nothing, i) -> Right (boardname, i)
            Right (Just b, i) -> Right (b, i)


postDependencies :: Post -> [ PostId ]
postDependencies = undefined


postsDeps :: [[ Post ]] -> PostWithDeps
postsDeps = undefined


indexPosts :: [ PostWithDeps ] -> Map PostId PostWithDeps
indexPosts = foldl' asdf empty
    where
        asdf :: Map PostId PostWithDeps -> PostWithDeps -> Map PostId PostWithDeps
        asdf = undefined

{-
indexPosts :: [[ Post ]] -> Map Int Post
indexPosts = foldl' asdf empty
    where
        asdf :: Map Int Post -> [ Post ] -> Map Int Post
        asdf = undefined
-}
