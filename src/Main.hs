{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- base
import Control.Applicative ( empty, (<|>), many )
-- import Control.Monad ( void )
import Data.Char ( isAlpha, isSpace )
import Data.Either ( partitionEithers )
import Data.List ( sort )
import Data.Void ( Void )
import System.IO ( stdin )
import System.Exit ( die )

-- containers
import Data.Map ( Map )
import qualified Data.Map as Map

-- megaparsec
import Text.Megaparsec (initialPos, eof, setPosition, parseErrorPretty, parse, SourcePos,  Parsec, (<?>), takeWhile1P )
import Text.Megaparsec.Char ( space1 )
import qualified Text.Megaparsec.Char.Lexer as L

-- parser-combinators
import Control.Applicative.Combinators ( sepBy1 )

-- text
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.IO




-- | SMTLib expressions (Lisp-like S-Expressions)
data Expr a
  = Value a
  | SExpr [Expr a]
  deriving (Eq, Show, Functor)

-- | Type declarations of constants, functions, ...
newtype Declaration = Decl { unDecl :: Expr Text }
  deriving (Show)

-- | A first-order formula
newtype Formula = Formula { unFormula :: Expr Text }
  deriving (Show)

-- | Id of a statement
newtype Id = Id { unId :: Integer }
  deriving (Eq, Ord, Show)

-- | A statement of the proof (axiom or inferred from previous statements)
data Statement
  = Axiom Formula
  | Inference Formula [Id]
  deriving (Show)

data Proof = Proof
  { proofDeclarations :: [Declaration]
  , proofStatements :: Map Id Statement
  }
  deriving (Show)





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
    value = lexeme $ Value <$> takeWhile1P Nothing isValueChar
    isValueChar c = not (isSpace c) && c /= '(' && c /= ')'

idP :: Parser Id
idP = Id <$> lexeme L.decimal

declP :: Parser Declaration
declP = Decl <$> sexpr

formulaP :: Parser Formula
formulaP = Formula <$> sexpr

statementP :: Parser (Id, Statement)
statementP = do
  stmtId <- idP <* symbol "."
  stmtConclusion <- formulaP
  let wordP = lexeme $ takeWhile1P Nothing isAlpha
      premise = (Right <$> idP) <|> (Left <$> wordP)
  stmtPremises <- symbol "[" *> sepBy1 premise (symbol ",") <* symbol "]"
  let stmt' = case stmtPremises of
                [ Left "axiom" ] -> Right $ Axiom stmtConclusion
                ps -> case sequenceA ps of
                        Left txt -> Left $ "unexpected: " <> Text.unpack txt
                        Right premiseIds -> Right $ Inference stmtConclusion premiseIds
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

hasDuplicates :: forall a . Ord a => [a] -> Bool
hasDuplicates = go . sort
  where go :: [a] -> Bool
        go (x:xs@(y:_)) = x == y || go xs
        go _ = False

findDuplicate :: forall a . Ord a => [a] -> Maybe a
findDuplicate = go . sort
  where go :: [a] -> Maybe a
        go (x:xs@(y:_))
          | x == y    = Just x
          | otherwise = go xs
        go _ = Nothing

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




main :: IO ()
main = do
  input <- Data.Text.IO.hGetContents stdin
  case parseProof input of
    Left err -> die $ "unable to parse input: " <> err
    Right proof -> print proof
