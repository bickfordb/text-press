module Press.Render where

import Control.Monad.State
import Data.Map (Map, lookup, fromList, insert)
import Data.Maybe (listToMaybe, catMaybes)
import Prelude hiding (lookup)

import Text.JSON.Types 

import Press.Types

instance Render Node where 
    render (Text s) = return s 
    render (Var var) = do
        context <- get
        case lookupVar var context of
            Nothing -> return ""
            Just jsval -> return $ show jsval
    render (Tag _ f) = render f 

instance Render TagFunc where
    render (TagFunc f) = f 

lookupVar name (RenderState {renderStateValues = vals}) = 
    listToMaybe . catMaybes $ map (getf name) vals

getf name (JSObject a) = get_field a name 
getf name otherwise = Nothing

-- Show a block
showBlock blockName = do
    templates <- templateStack
    let maybeNodes = lookupFirst blockName $ map tmplBlocks $ templates
    case maybeNodes of
        Just nodes -> fmap (foldl (++) "") $ mapM render nodes
        Nothing -> return ""

lookupFirst :: Ord k => k -> [Map k a] -> Maybe a
lookupFirst name maps = listToMaybe . catMaybes $ map (lookup name) maps 

getTemplate = fmap renderStateTemplate get

templateStack = getTemplate >>= templateStack' 
    where
        templateStack' t@(Template {tmplExtends=Nothing}) = return [t]
        templateStack' t@(Template {tmplExtends=Just name}) = do
            cache <- fmap (parserTemplateCache . renderStateParser) get
            case lookup name cache of
                Just template -> do
                    templates <- templateStack' template
                    return $ (template : templates)
                Nothing -> liftIO $ error $ "expecting a template"

doRender = do 
    bodyNodes <- fmap (tmplNodes . last) templateStack
    st <- get
    fmap (foldl (++) "") $ mapM render bodyNodes
        
    

