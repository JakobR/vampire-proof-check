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

-- vampire-proof-checker
import VampireProofChecker.Parser ( parseProof )
import VampireProofChecker.Types



main :: IO ()
main = do
  input <- Data.Text.IO.hGetContents stdin
  case parseProof input of
    Left err -> die $ "unable to parse input: " <> err
    Right proof -> print proof
