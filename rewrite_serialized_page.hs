main :: IO ()
main =
    readFile "/tmp/asdf.html" >>= (return . (read :: String -> String)) >>= (writeFile "/tmp/asdf2.html")
