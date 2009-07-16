module Press.Run where

import Control.Monad.State
import Prelude hiding (lookup)

import Data.Data (Data)
import Data.Map (insert, lookup)
import Text.JSON.Generic (toJSON) 
import Text.JSON (JSValue)

import Press.Types
import Press.Parser
import Press.Render
import Press.Tags

run :: Data a => [a] -> TemplatePath -> IO String
run datas templateName = evalStateT (run' jsonDatas templateName) defaultParser
    where jsonDatas = map toJSON datas

run' :: [JSValue] -> TemplatePath -> StateT Parser IO String
run' datas templateName = do
    addToTemplateCache templateName
    parser <- get
    template <- fmap head $ lookupTemplates templateName
    let st = RenderState {
        renderStateParser = parser,
        renderStateTemplate = template,
        renderStateValues = datas
    }
    liftIO $ evalStateT doRender st 

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
