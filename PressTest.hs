module Main where

import Data.List

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit

import Text.Press.Parser
import Text.Press.Run
import Text.Press.Render
import Text.JSON (decodeStrict, Result(..))

main = defaultMain tests

tests = [
    testGroup "Parser" [
        testCase "Parse an empty file" testParseEmpty
        , testCase "Parse an empty string" $ parses ""
        , testCase "Parse a newline" $ parses "\n"
        , testCase "Parse a var" $ parses "{{x}}"
        , testCase "Parse extends" $ parses "{% extends \"foo.html\" %}"
        , testCase "Parse block" $ parses "{% block x%}a{% endblock %}"
        , testCase "Parse if" $ parses "{%if x%}x{%endif%}"
        , testCase "Parse if else" $ parses "{%if x%}a{%else%}b{%endif%}"
        , testCase "Parse if elif else" $ parses "{%if x%}a{%elif y%}b{%else%}c{%endif%}"
        , testCase "Parse if elif" $ parses "{%if x%}a{%elif y%}b{%endif%}"
        , testCase "Parse for" $ parses "{%for x in y%}{{x}}{%endfor%}"
        , testCase "Parse comment" $ parses "{%comment%}{%endcomment%}"
        , testCase "Parse cycle 1" $ parses "{%cycle row1, row2, row3 as rowcolor%}"
        , testCase "Parse cycle 2" $ parses "{%cycle rowcolors %}"
        , testCase "Parse variable with filters" $ parses "{{x | strip}}"
        ]
    , testGroup "Renderer" [
        testCase "Render a var" $ rendersTo "{{x}}" "{\"x\":1}" "1"
        , testCase "Render a block" $ rendersTo "{% block x %}a{% endblock %}" "{}" "a"
        ]
    ]

testParseEmpty = assertParseFile "test-data/empty.html" >> return ()

assertParseFile f = assertRight $ parseFile defaultParser f
assertRight action = do
    result <- action 
    case result of 
        Left err -> error $ show err
        Right x -> return () 

parses s = assertRight $ return $ parseString defaultParser s

renders_ tmpl json = renders tmpl json >> return ()

renders tmpl json = do
    case decodeStrict json of 
        Error e -> error . show $ e
        Ok a -> runJSONWithBody [a] tmpl 

assertEq left right 
    | left == right = return ()
    | otherwise = error $ "expecting " ++ (show left) ++ " == " ++ (show right)

rendersTo tmpl json expected = do 
    result <- renders tmpl json
    assertEq expected result