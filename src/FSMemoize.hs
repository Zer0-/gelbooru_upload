{-# LANGUAGE NoImplicitPrelude #-}

module FSMemoize
    ( fsmemoize
    ) where

import Prelude hiding (readFile, writeFile)
import Data.ByteString (ByteString, readFile, writeFile)
import System.FilePath (FilePath, (</>))
import System.Directory (doesFileExist)

fsmemoize
    :: FilePath
    -> (a -> FilePath)
    -> (a -> IO ByteString)
    -> a
    -> IO ByteString
fsmemoize datadir keyfun costlyOp args = do
    exists <- doesFileExist path 
    if exists
    then readFile path
    else do
        bs <- costlyOp args
        writeFile path bs
        return bs

    where
        path = datadir </> (keyfun args)
