module Retopo
    ( indexPosts
    ) where

import Types
    ( --PostWithDeps
        Post
    )

import Data.List (foldl')
import Data.Map (Map, empty)


{-
indexPosts :: [ PostWithDeps ] -> Map Int PostWithDeps
indexPosts = foldl' asdf empty
    where
        asdf :: Map Int PostWithDeps -> PostWithDeps -> Map Int PostWithDeps
        asdf = undefined
-}

indexPosts :: [[ Post ]] -> Map Int Post
indexPosts = foldl' asdf empty
    where
        asdf :: Map Int Post -> [ Post ] -> Map Int Post
        asdf = undefined
