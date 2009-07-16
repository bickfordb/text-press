{-# LANGUAGE DeriveDataTypeable #-}

module Main where
import Data.Data
import Data.Map

import Press.Run

data Foo = Foo {hi :: String} 
    deriving (Data, Typeable)

main = do 
    s <- run [Foo {hi="there"}] "example4.html"
    print s

