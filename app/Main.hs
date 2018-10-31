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
