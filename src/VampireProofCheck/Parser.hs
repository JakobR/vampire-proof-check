{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VampireProofCheck.Parser
  ( parseProof
  ) where

-- base
import Control.Applicative (empty, (<|>), many)
import Data.Char (isAlpha, isSpace)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.List (intercalate)
import Data.Void (Void)

-- containers
import qualified Data.Map.Strict as Map

-- megaparsec
import Text.Megaparsec
  (getOffset, setOffset,  initialPos, eof, errorBundlePretty
  , parse, SourcePos(..), Parsec, (<?>), takeWhile1P
  , State(..), PosState(..), updateParserState
  )
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

-- mtl
import Control.Monad.Except (MonadError(..))

-- parser-combinators
import Control.Applicative.Combinators (sepBy)

-- safe
import Safe (headMay)

-- text
import Data.Text (Text)
import qualified Data.Text as Text

-- vampire-proof-check
import qualified Data.DependencyGraph as DependencyGraph
import VampireProofCheck.List (findDuplicate')
import VampireProofCheck.Types


type Parser = Parsec Void Text

-- | space consumer, skips whitespace and comments
spc :: Parser ()
spc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment ";"
    blockComment = empty  -- no support for block comments atm

-- | @lexeme p@ parses p and consumes all following whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spc

lexemeNamed :: String -> Parser a -> Parser a
lexemeNamed s p = lexeme p <?> s

-- | Parse symbols (i.e., verbatim strings)
symbol :: Text -> Parser Text
symbol = L.symbol spc

sexpr :: Parser (Expr Text)
sexpr = SExpr <$> (symbol "(" *> many expr <* symbol ")")

expr :: Parser (Expr Text)
expr = sexpr <|> value
  where
    value = lexemeNamed "expr-value" $ Value <$> takeWhile1P Nothing isValueChar
    isValueChar c = not (isSpace c) && c /= '(' && c /= ')'

idP :: Parser Id
idP = Id <$> lexemeNamed "id" L.decimal

declP :: Parser Declaration
declP = Decl <$> sexpr

formulaP :: Parser Formula
formulaP = Formula <$> sexpr

statementP :: Parser (Ann Offset Id, StatementF (Ann Offset Id))
statementP = do
  stmtId <- located idP <* symbol "."
  conclusion <- formulaP
  let wordP = lexemeNamed "premise-word" $ takeWhile1P Nothing isAlpha
      premise = (Right <$> located idP) <|> (Left <$> wordP)
  premises <- symbol "[" *> sepBy premise (symbol ",") <* symbol "]"
  let stmt' = case premises of
                [ Left "axiom" ] -> Right $ Axiom conclusion
                ps -> case sequenceA ps of
                        Left txt -> Left $ "unexpected: " <> Text.unpack txt
                        Right premiseIds -> Right $ Inference conclusion premiseIds
  case stmt' of
    Left err -> fail err
    Right stmt -> return (stmtId, stmt)

data Ann ann a = Ann
  { annotation :: !ann
  , payload :: !a
  }

-- | NOTE: compares only the payload!
instance Eq a => Eq (Ann ann a) where
  (==) = (==) `on` payload

-- | NOTE: compares only the payload!
instance Ord a => Ord (Ann ann a) where
  compare = compare `on` payload

newtype Offset = Offset Int

located :: Parser a -> Parser (Ann Offset a)
located p = do
  o <- getOffset
  x <- p
  return (Ann (Offset o) x)

proofP :: Parser Proof
proofP = do
  let line = (Left <$> declP) <|> (Right <$> statementP)
  (proofDeclarations, annStmts) <- partitionEithers <$> many line

  case findDuplicate' (fst <$> annStmts) of
    Just (_, Ann o dupId) -> failAt o $ "duplicate id: " <> show dupId
    Nothing -> return ()

  case DependencyGraph.resolve (Map.fromList annStmts) of
    Left (DependencyGraph.Missing (Ann o i) (Ann _ j)) ->
      failAt o $ "statement " <> show j <> " depends on non-existing premise " <> show i
    Left (DependencyGraph.Cycle ann_js) -> do
      let js = payload <$> ann_js
          jcycle = js ++ take 1 js
          o = annotation <$> headMay ann_js
      failAtMay o $ "circular dependency between statements: " <> intercalate " -> " (show <$> jcycle)
    Right annProofStatements -> do
      let proofStatements = Map.mapKeysMonotonic payload annProofStatements
      return Proof{..}

failAt :: Offset -> String -> Parser a
failAt (Offset o) msg = setOffset o >> fail msg

failAtMay :: Maybe Offset -> String -> Parser a
failAtMay (Just o) = failAt o
failAtMay Nothing = fail

setPosition :: SourcePos -> Parser ()
setPosition pos = updateParserState $ \s -> s{statePosState = (statePosState s){ pstateSourcePos = pos }}

parseProof'
  :: MonadError String m
  => SourcePos
  -> Text
  -> m Proof
parseProof' pos str =
  case parse p (sourceName pos) str of
    Left err -> throwError $ errorBundlePretty err
    Right x -> return x
  where
    p :: Parser Proof
    p = do setPosition pos
           spc  -- consume leading spaces
           prf <- proofP
           eof
           return prf

parseProof
  :: MonadError String m
  => String  -- ^ name of source file
  -> Text    -- ^ text to parse
  -> m Proof
parseProof srcName = parseProof' (initialPos srcName)
