module Text.Press.Run where

import Control.Monad.State
import Control.Monad.Error (runErrorT, ErrorT)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Writer.Lazy
import Prelude hiding (lookup)

import Data.Data (Data)
import Data.Map (insert, lookup)
import Text.JSON.Generic (toJSON) 
import Text.JSON (JSValue, decodeStrict)

import Text.Press.Types
import Text.Press.Parser
import Text.Press.Render
import Text.Press.Tags

type Result = Either PressError [String]

runDataWithPath :: Data a => [a] -> String -> IO Result
runDataWithPath datas templateName = runJSONWithPath jsonDatas templateName
    where jsonDatas = map toJSON datas

runDataWithBody :: Data a => [a] -> String -> IO Result
runDataWithBody datas body = runJSONWithBody jsonDatas body
    where jsonDatas = map toJSON datas

runJSONWithPath datas templateName = runErrorT $ evalStateT (runJSONWithPathStTErrT datas templateName) defaultParser

runJSONWithPathStTErrT ::  [JSValue] -> String -> StateT Parser (ErrorT PressError IO) [String]
runJSONWithPathStTErrT datas templateName = do
    addToTemplateCache templateName
    parser <- get
    template <- fmap head $ lookupTemplates templateName
    let st = RenderState {
        renderStateParser = parser,
        renderStateTemplate = template,
        renderStateValues = datas
    }
    lift $ evalStateT (execWriterT doRender) st

runJSONWithBody :: [JSValue] -> String -> IO Result
runJSONWithBody jsvalues body = do 
    case parseString defaultParser body of
        Left parsecError -> return $ Left $ ParseError parsecError
        Right template -> runErrorT $ evalStateT (runJSONWithTemplateStTErrT jsvalues template) defaultParser

runJSONWithTemplate :: [JSValue] -> Template -> Parser -> IO Result
runJSONWithTemplate jsvalues template parser = runErrorT $ evalStateT (runJSONWithTemplateStTErrT jsvalues template) parser

runJSONWithTemplateStTErrT :: [JSValue] -> Template -> StateT Parser (ErrorT PressError IO) [String]
runJSONWithTemplateStTErrT jsvalues template = do
    case tmplExtends template of 
        Just s -> addToTemplateCache s
        Nothing -> return ()
    parser <- get
    lift $ evalStateT (execWriterT doRender) RenderState {
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
        Nothing -> throwError $ PressError $ "unexpected uncached template: " ++ (show templateName)

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
