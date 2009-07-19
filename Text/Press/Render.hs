module Text.Press.Render where

import Control.Monad.State
import Control.Monad.Writer.Lazy
import Control.Monad.Error.Class (throwError)

import Data.Map (Map, lookup, fromList, insert)
import Data.Maybe (listToMaybe, catMaybes)
import Prelude hiding (lookup)

import Text.JSON.Types 
import Text.JSON

import Text.Press.Types

emit s = tell [s]

instance Render Node where 
    render (Text s) = emit s
    render (Var var) = do
        context <- getRenderState
        case lookupVar var context of
            Nothing -> emit ""
            Just jsval -> render jsval
    render (Tag _ f) = render f 

instance Render TagFunc where
    render (TagFunc f) = f 

instance Render JSValue where 
    render JSNull = emit ""
    render (JSString x) = emit $ fromJSString x
    render other = emit $ (showJSValue other) ""

lookupVarM name = do 
    st <- getRenderState 
    return $ lookupVar name st

lookupVar name (RenderState {renderStateValues = vals}) = 
    listToMaybe . catMaybes $ map (getf name) vals

getf name (JSObject a) = get_field a name 
getf name otherwise = Nothing

-- Show a block
showBlock :: String -> RenderT_ 
showBlock blockName = do
    templates <- templateStack
    let maybeNodes = lookupFirst blockName $ map tmplBlocks $ templates
    case maybeNodes of
        Just nodes -> mapM_ render nodes
        Nothing -> tell [""]

lookupFirst :: Ord k => k -> [Map k a] -> Maybe a
lookupFirst name maps = listToMaybe . catMaybes $ map (lookup name) maps 

getTemplate = fmap renderStateTemplate getRenderState

templateStack = getTemplate >>= templateStack' 
    where
        templateStack' t@(Template {tmplExtends=Nothing}) = return [t]
        templateStack' t@(Template {tmplExtends=Just name}) = do
            cache <- fmap (parserTemplateCache . renderStateParser) get
            case lookup name cache of
                Just template -> do
                    templates <- templateStack' template
                    return $ t : (template : templates)
                Nothing -> throwError $ PressError $ "expecting a template in the cache named: " ++ (show name)

doRender = do 
    bodyNodes <- fmap (tmplNodes . last) templateStack
    mapM render bodyNodes

coerceJSToBool :: JSValue -> Bool
coerceJSToBool JSNull = False 
coerceJSToBool (JSBool bool) = bool
coerceJSToBool (JSRational sign r) = (not sign) && (r > 0)
coerceJSToBool (JSString x) = length (fromJSString x) > 0
coerceJSToBool (JSArray vals) = length vals > 0
coerceJSToBool (JSObject obj) = length (fromJSObject obj) > 0
