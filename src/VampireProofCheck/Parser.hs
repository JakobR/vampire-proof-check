{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VampireProofCheck.Parser
  ( parseProof
  ) where

-- base
import Control.Applicative ( empty, (<|>), many )
import Data.Char ( isAlpha, isSpace )
import Data.Either ( partitionEithers )
import Data.Maybe ( fromMaybe )
import Data.Void ( Void )

-- containers
import qualified Data.Map.Strict as Map

-- megaparsec
import Text.Megaparsec
  ( initialPos, eof, errorBundlePretty
  , parse, SourcePos, Parsec, (<?>), takeWhile1P
  , State(..), PosState(..), updateParserState
  )
import Text.Megaparsec.Char ( space1 )
import qualified Text.Megaparsec.Char.Lexer as L

-- mtl
import Control.Monad.Except ( MonadError(..) )

-- parser-combinators
import Control.Applicative.Combinators ( sepBy )

-- text
import Data.Text ( Text )
import qualified Data.Text as Text

-- vampire-proof-check
import VampireProofCheck.List ( findDuplicate )
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

statementP :: Parser (Id, Statement)
statementP = do
  stmtId <- idP <* symbol "."
  conclusion <- formulaP
  let wordP = lexemeNamed "premise-word" $ takeWhile1P Nothing isAlpha
      premise = (Right <$> idP) <|> (Left <$> wordP)
  premises <- symbol "[" *> sepBy premise (symbol ",") <* symbol "]"
  let stmt' = case premises of
                [ Left "axiom" ] -> Right $ Axiom conclusion
                ps -> case sequenceA ps of
                        Left txt -> Left $ "unexpected: " <> Text.unpack txt
                        Right premiseIds -> Right $ Inference conclusion premiseIds
  case stmt' of
    Left err -> fail err
    Right stmt -> return (stmtId, stmt)

proofP :: Parser Proof
proofP = do
  let line = (Left <$> declP) <|> (Right <$> statementP)
  (proofDeclarations, stmts) <- partitionEithers <$> many line
  let ids = fst <$> stmts
  case findDuplicate ids of
    Just dupId -> fail $ "duplicate id: " <> show dupId
    Nothing -> return ()
  let proofStatements = Map.fromList stmts
  return Proof{..}

setPosition :: SourcePos -> Parser ()
setPosition pos = updateParserState $ \s -> s{statePosState = (statePosState s){ pstateSourcePos = pos }}

parseProof'
  :: MonadError String m
  => SourcePos
  -> Text
  -> m Proof
parseProof' pos str =
  case parse p "" str of
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
  => Maybe FilePath  -- ^ name of source file
  -> Text            -- ^ text to parse
  -> m Proof
parseProof srcName = parseProof' (initialPos $ fromMaybe "<string>" srcName)
