{-# LANGUAGE ScopedTypeVariables #-}

module Text.Press.Tags where
import Text.JSON.Types

import Data.Map (fromList, insert)
import Data.Maybe (catMaybes)
import qualified Text.Parsec.Prim as Parsec.Prim
import Text.Parsec.Combinator (manyTill, choice)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad (forM_)

import Text.Press.Parser
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

-- This is mapping of all of the default tag types. 
defaultTagTypes = (fromList [
    ("extends", TagType extendsTag),
    ("block", TagType blockTag),
    ("if", TagType ifTag),
    ("for", TagType forTag),
    ("comment", TagType commentTag)
    ])

-- Comment Tag
commentTag name rest = do 
    manyTill pNode (tagNamed "endcomment")
    return Nothing

-- If tag
ifTag name rest = do
        expr <- parseIfExpr rest
        scan [] expr
    where
        scan ifs e = do
            (maybeNodes, tokenPos) <- manyTill' pNode (tagNamedOneOf ["else", "endif", "elif"])
            let nodes = catMaybes maybeNodes
            let token = fst tokenPos
            let ifs' = ifs ++ [(e, nodes)]
            case token of
                (PTag "endif" rest) -> do
                    return $ Just $ Tag "if" $ TagFunc $ showIfElse ifs' []
                (PTag "elif" rest) -> do
                    e' <- parseIfExpr rest
                    scan ifs' e'
                (PTag "else" rest) -> do
                    nodes <- fmap catMaybes $ manyTill pNode (tagNamed "endif")
                    return $ Just $ Tag "if" $ TagFunc $ showIfElse ifs' nodes
                otherwise -> Parsec.Prim.unexpected "unexpected tag"
        parseIfExpr s = do
            exprs <- runParseTagExpressions s
            case exprs of
                [] -> Parsec.Prim.unexpected "empty if"
                (x : []) -> return x
                (x : xs) -> Parsec.Prim.unexpected $ show . head $ xs

-- Version of manyTill that returns the terminating token
manyTill' p1 p2 = scan
    where scan = (Parsec.Prim.try p2') Parsec.Prim.<|> p1'
          p1' = do x <- p1
                   (xs, y) <- scan
                   return (x : xs, y)
          p2' = do y <- p2
                   return ([], y)

-- Evaluate an expression to a boolean suitable for an If clase
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

showIfElse :: [(Expr, [Node])] -> [Node] -> RenderT_
showIfElse [] right = mapM_ render right
showIfElse ((expr, left) : xs) right = do
    succ <- exprToBool expr
    if succ
        then mapM_ render left
        else showIfElse xs right

forTag name rest = do
    (target, sourceExpr) <- parseFor rest
    (maybeNodes, (token, pos)) <- manyTill' pNode (tagNamedOneOf ["endfor", "else"])
    let forNodes = catMaybes maybeNodes
    case token of
        PTag "else" _ -> do
            elseNodes <- fmap catMaybes $ manyTill pNode (tagNamed "endfor")
            return $ Just $ Tag "for" $ TagFunc $ showFor target sourceExpr forNodes elseNodes
        PTag "endfor" _ -> do
            return $ Just $ Tag "for" $ TagFunc $ showFor target sourceExpr forNodes []

    where
        parseFor s = do
             exprs <- runParseTagExpressions s
             if length exprs == 3
                then do
                    target <- case head exprs of
                        ExprVar x -> return x
                        otherwise -> fail "unexpected for target"
                    case head $ tail exprs of
                        ExprVar "in" -> return ()
                        otherwise -> fail "expecting 'in'"
                    return $ (target, head $ tail $ tail exprs)
                else Parsec.Prim.unexpected "number of arguments"

showFor :: String -> Expr -> [Node] -> [Node] -> RenderT_
showFor target sourceExpr forNodes elseNodes = do
        sourceValues <- fmap toList $ toJS sourceExpr
        runFor sourceValues
    where
        runFor [] = mapM_ render elseNodes
        runFor vals = forM_ vals $ \x -> do
            pushValues [(target, x)]
            mapM_ render forNodes
            popValues

        toList (Just (JSArray vals)) = vals
        toList otherwise = []

        toJS (ExprVar x) = lookupVarM x
        toJS otherwise = return Nothing

        pushValues kvpairs = do
            st <- getRenderState
            setRenderState $ st {
                renderStateValues = values' : renderStateValues st
            }
            where values' = JSObject $ toJSObject kvpairs

        popValues = do
            st <- getRenderState
            setRenderState $ st {
                renderStateValues = tail $ renderStateValues st
            }

