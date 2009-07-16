module Press.Tags where

import Data.Map (fromList, insert)
import Data.Maybe (catMaybes)
import qualified Text.Parsec.Prim as Parsec.Prim
import Text.Parsec.Combinator (manyTill)

import Press.Parser
import Press.Render
import Press.Types

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
    ("block", TagType blockTag)
    ])

