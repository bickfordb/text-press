module Main where

import Text.Press.Parser 
import Text.Press.Types 
import Text.Press.Run

main = do 
    let parser = defaultParser 
    s <- parseFile parser "example2.html"
    print s
    s <- parseFile parser "example3.html"
    print s
    s <- parseFile parser "example.html"
    print s
    return ()

