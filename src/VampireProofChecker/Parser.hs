{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VampireProofChecker.Parser
  ( parseProof
  ) where

-- base
import Control.Applicative ( empty, (<|>), many )
import Data.Char ( isAlpha, isSpace )
import Data.Either ( partitionEithers )
import Data.Void ( Void )

-- containers
import qualified Data.Map as Map

-- megaparsec
import Text.Megaparsec
  ( initialPos, eof, setPosition, parseErrorPretty
  , parse, SourcePos, Parsec, (<?>), takeWhile1P
  )
import Text.Megaparsec.Char ( space1 )
import qualified Text.Megaparsec.Char.Lexer as L

-- parser-combinators
import Control.Applicative.Combinators ( sepBy1 )

-- text
import Data.Text ( Text )
import qualified Data.Text as Text

-- vampire-proof-checker
import VampireProofChecker.List ( findDuplicate )
import VampireProofChecker.Types


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
  premises <- symbol "[" *> sepBy1 premise (symbol ",") <* symbol "]"
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

parseProof' :: SourcePos -> Text -> Either String Proof
parseProof' pos str =
  case parse p "" str of
    Left err -> Left $ parseErrorPretty err
    Right e -> return e
  where
    p :: Parser Proof
    p = do setPosition pos
           spc  -- consume leading spaces
           prf <- proofP
           eof
           return prf

parseProof :: Text -> Either String Proof
parseProof = parseProof' (initialPos "<string>")
