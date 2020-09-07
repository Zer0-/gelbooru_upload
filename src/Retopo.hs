module Retopo
    ( indexPosts
    ) where

import Types
    ( PostWithDeps
    , Post (..)
    )

import Data.List (foldl')
import Data.Map (Map, empty)

type PostId = (String, Int) -- String is the board name

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
