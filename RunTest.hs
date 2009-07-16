{-# LANGUAGE DeriveDataTypeable #-}

module Main where
import Data.Data
import Data.Map

import Text.Press.Run

data Foo = Foo {hi :: String} 
    deriving (Data, Typeable)

main = do 
    s <- run [Foo {hi="there"}] "example4.html"
    putStrLn s

