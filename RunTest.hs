{-# LANGUAGE DeriveDataTypeable #-}

module Main where
import Data.Data
import Data.Map

import Text.Press.Run

data Foo = Foo {hi :: String, number :: Double, bool:: Bool} 
    deriving (Data, Typeable)

main = do 
    s <- run [Foo {hi="there", number=5.12345678901234, bool=False}] "example4.html"
    putStrLn s

