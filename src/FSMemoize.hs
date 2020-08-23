module FSMemoize
    ( fsmemoize
    ) where

import Prelude hiding (readFile, writeFile)
import System.FilePath (FilePath, (</>))
import System.Directory (doesFileExist)
import Data.Serialize (Serialize, encode, decode)
import Data.ByteString (readFile, writeFile)
import Data.Either (fromRight)

fsmemoize
    :: Serialize b
    => FilePath
    -> (a -> FilePath)
    -> (a -> IO b)
    -> a
    -> IO b
fsmemoize datadir keyfun costlyOp args = do
    exists <- doesFileExist path 
    if exists
    then readFile path >>= return . (fromRight undefined) . decode
    else do
        bs <- costlyOp args
        writeFile path $ encode bs
        return bs

    where
        path = datadir </> (keyfun args)
