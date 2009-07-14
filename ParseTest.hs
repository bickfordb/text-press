module Main where

import Press.Parser 

main = do 
    let parser = newParser 
    s <- parseFile parser "example2.html"
    print s
    s <- parseFile parser "example3.html"
    print s
    s <- parseFile parser "example.html"
    print s
    return ()

