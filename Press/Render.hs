module Press.Render where

import Prelude hiding (lookup)
import Data.Map (Map, lookup)
import Data.Maybe (listToMaybe, catMaybes)

import Press.Types

instance Render Node where 
    render context (Text s) = return s 
    render context (Var var) = do
        case lookupVar var context of
            Nothing -> return ""
            Just jsval -> return $ show jsval
    render context (Tag _ f) = render context f 

instance Render TagFunc where
    render context (TagFunc f) = f context

lookupVar name (RenderContext{renderContextValues=vals}) = getFirst name vals 

showBlock blockName ctx = do
    let maybeNodes = getFirst blockName $ map tmplBlocks $ renderContextTemplateStack ctx
    case maybeNodes of
        Just nodes -> fmap (\x -> foldr (++) "" x) $ mapM (render ctx) nodes
        Nothing -> return ""

getFirst :: Ord k => k -> [Map k a] -> Maybe a
getFirst name maps = listToMaybe . catMaybes $ map (lookup name) maps 

