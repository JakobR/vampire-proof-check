{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- base
import Data.List ( sort )
import System.IO ( hPrint, stderr )
import System.Exit ( die )

-- containers
import Data.Map ( Map )
import qualified Data.Map as Map

-- text
import Data.Text ( Text )
import qualified Data.Text.IO

-- vampire-proof-checker
import VampireProofChecker.Options
import VampireProofChecker.Parser ( parseProof )
import VampireProofChecker.Types




main :: IO ()
main = do
  opts@Options{..} <- execOptionsParser
  hPrint stderr opts
  input <- readInput optProofFile
  case parseProof input of
    Left err -> die $ "unable to parse input: " <> err
    Right proof -> checkProof opts proof

-- | Read all data from file or stdin
readInput :: Maybe FilePath -> IO Text
readInput Nothing = Data.Text.IO.getContents
readInput (Just file) = Data.Text.IO.readFile file

checkProof :: Options -> Proof -> IO ()
checkProof opts proof = do
  print "ho"
