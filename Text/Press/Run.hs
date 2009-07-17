module Text.Press.Run where

import Control.Monad.State
import Prelude hiding (lookup)

import Data.Data (Data)
import Data.Map (insert, lookup)
import Text.JSON.Generic (toJSON) 
import Text.JSON (JSValue, decodeStrict)

import Text.Press.Types
import Text.Press.Parser
import Text.Press.Render
import Text.Press.Tags

runDataWithPath :: Data a => [a] -> String -> IO String
runDataWithPath datas templateName = evalStateT (runJSONWithPath jsonDatas templateName) defaultParser
    where jsonDatas = map toJSON datas

runDataWithBody :: Data a => [a] -> String -> IO String
runDataWithBody datas body = runJSONWithBody jsonDatas body
    where jsonDatas = map toJSON datas

runJSONWithPath :: [JSValue] -> String -> StateT Parser IO String
runJSONWithPath datas templateName = do
    addToTemplateCache templateName
    parser <- get
    template <- fmap head $ lookupTemplates templateName
    let st = RenderState {
        renderStateParser = parser,
        renderStateTemplate = template,
        renderStateValues = datas
    }
    liftIO $ evalStateT doRender st 

runJSONWithBody :: [JSValue] -> String -> IO String
runJSONWithBody jsvalues body = do 
    case parseString defaultParser body of
        Left err -> error $ show err
        Right template -> runJSONWithTemplate jsvalues template defaultParser

runJSONWithTemplate :: [JSValue] -> Template -> Parser -> IO String
runJSONWithTemplate jsvalues template parser = evalStateT (runJSONWithTemplate' jsvalues template) parser

runJSONWithTemplate' jsvalues template = do
    case tmplExtends template of 
        Just s -> addToTemplateCache s
        Nothing -> return ()
    parser <- get
    liftIO $ evalStateT doRender $ RenderState {
        renderStateParser = parser,
        renderStateTemplate = template,
        renderStateValues = jsvalues
    }

lookupTemplates templateName = do
    parser <- get
    case lookup templateName (parserTemplateCache parser) of
        Just template -> case tmplExtends template of 
            Nothing -> return [template]
            Just s -> do 
                xs <- lookupTemplates s
                return $ template : xs
        Nothing -> liftIO $ error $ "unexpected uncached template: " ++ (show templateName)


addToTemplateCache template = do
    parser <- get
    let mapping = parserTemplateCache parser
    case lookup template mapping of 
        Just tmpl -> return ()
        Nothing -> do
            eitherTemplateError <- liftIO $ parseFile parser template
            case eitherTemplateError of
                Left err -> liftIO $ error . show $ err
                Right tmpl -> do 
                    let mapping' = insert template tmpl mapping 
                    put $ parser {parserTemplateCache = mapping'}
                    case tmplExtends tmpl of
                        Nothing -> return ()
                        Just s -> addToTemplateCache s

defaultParser = newParser { parserTagTypes = defaultTagTypes }
