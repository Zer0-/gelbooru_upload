import Prelude hiding (writeFile)
import System.Environment (getArgs)
import Data.ByteString (ByteString, writeFile)

main :: IO ()
main = do
    args <- getArgs
    let file = head args
    putStrLn file

    readFile file >>= (return . (read :: String -> ByteString)) >>= (writeFile "/home/phil/tmp/asdf2.jpg")
