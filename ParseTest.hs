module Main where

import Press.Parser 
import Press.Types 
import Press.Run

main = do 
    let parser = defaultParser 
    s <- parseFile parser "example2.html"
    print s
    s <- parseFile parser "example3.html"
    print s
    s <- parseFile parser "example.html"
    print s
    return ()

