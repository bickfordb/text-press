module Text.Press.Tags where
import Text.JSON.Types

import Data.Map (fromList, insert)
import Data.Maybe (catMaybes)
import qualified Text.Parsec.Prim as Parsec.Prim
import Text.Parsec.Combinator (manyTill)
import Control.Monad.Trans (lift, liftIO)

import Text.Press.Parser
import Text.Press.Render
import Text.Press.Types

extendsTag name rest = do
    exprs <- runParseTagExpressions rest
    include <- case exprs of 
                (ExprStr s : xs) -> return s
                otherwise -> fail "expecting a string"
    let rest' = Just . strip $ include
    Parsec.Prim.modifyState $ \(parser, tmpl) -> (parser, tmpl {tmplExtends = rest'})
    return $ Nothing

blockTag name rest = do
    exprs <- runParseTagExpressions rest
    blockName <- case exprs of 
        (ExprVar var : xs) -> return var
        otherwise -> Parsec.Prim.unexpected (show otherwise)
    nodes <- fmap catMaybes $ manyTill pNode (tagNamed "endblock")
    Parsec.Prim.modifyState $ \(parser, tmpl) -> (parser,
        tmpl {tmplBlocks = insert blockName nodes (tmplBlocks tmpl)})
    return $ Just $ Tag "block" $ TagFunc $ showBlock blockName

defaultTagTypes = (fromList [
    ("extends", TagType extendsTag), 
    ("block", TagType blockTag),
    ("if", TagType ifTag)
    ])

ifTag name rest = do
    exprs <- runParseTagExpressions rest
    expr <- case exprs of 
        [] -> Parsec.Prim.unexpected "empty if"
        (x : []) -> return x
        (x : xs) -> Parsec.Prim.unexpected $ show . head $ xs

    nodes <- fmap catMaybes $ manyTill pNode (tagNamed "endif")
    return $ Just $ Tag "if" (TagFunc (showIf expr nodes))


exprToBool :: Expr -> RenderT Bool
exprToBool expr = do
    case expr of 
       ExprStr s -> return $ length s > 0 
       ExprNum num -> return $ num > 0 
       ExprVar var -> do
            maybeVal <- lookupVarM var
            case maybeVal of
                Nothing -> return False
                Just val -> return $ coerceJSToBool val

showIf :: Expr -> [Node] -> RenderT_
showIf expr nodes = do
    succ <- exprToBool expr
    if succ 
        then mapM_ render nodes
        else return ()
