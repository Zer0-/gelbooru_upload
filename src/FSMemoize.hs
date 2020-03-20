module FSMemoize
    ( fsmemoize
    ) where

import System.FilePath (FilePath, (</>))
import System.Directory (doesFileExist)

fsmemoize
    :: (Read b, Show b) => FilePath
    -> (a -> FilePath)
    -> (a -> IO b)
    -> a
    -> IO b
fsmemoize datadir keyfun costlyOp args = do
    exists <- doesFileExist path 
    if exists
    then readFile path >>= return . read
    else do
        bs <- costlyOp args
        writeFile path $ show bs
        return bs

    where
        path = datadir </> (keyfun args)
