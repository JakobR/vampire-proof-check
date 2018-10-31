{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- base
import Control.Monad ( forM_,  unless )
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
import VampireProofChecker.Vampire ( runVampire, VampireResult(..) )




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
checkProof opts proof@Proof{..} =
  forM_ (Map.keys proofStatements) $ \stmtId -> do
    result <- checkStatementId opts proof stmtId
    case result of
      True -> return ()
      False -> die $ "Check of statement " <> show stmtId <> " failed!"

checkStatementId
  :: Options
  -> Proof -- ^ the proof which is being checked
  -> Id    -- ^ Id of the statement that should be checked
  -> IO Bool
checkStatementId Options{..} Proof{..} checkId =
  case Map.lookup checkId proofStatements of
    Nothing -> fail $ "id not in db: " <> show checkId
    Just (Axiom _) ->
      -- Nothing to check for axioms
      return True
    Just (Inference conclusion premiseIds) -> do
      -- Inference may only depend on earlier statements
      unless (all (< checkId) premiseIds) $
        fail $ "inference depends on subsequent formula: " <> show checkId
      -- Look up premises and fail if one doesn't exist
      case sequenceA ((`Map.lookup` proofStatements) <$> premiseIds) of
        Nothing -> fail $ "inference depends on non-existing premise: " <> show checkId
        Just premises -> do
          let
            assertExpr :: Expr Text -> Expr Text
            assertExpr e = SExpr [ Value "assert", e ]
            premiseAssertions = assertExpr . unFormula . stmtConclusion <$> premises
            conclusionAssertion = SExpr [ Value "assert-not", unFormula conclusion ]
            exprs :: [Expr Text]
            exprs = (unDecl <$> proofDeclarations)
                    <> premiseAssertions
                    <> [conclusionAssertion]
          vampireResult <- runVampire optVampireExe optVampireTimeout exprs
          case vampireResult of
            Satisfiable -> return False
            Refutation -> return True
            Timeout -> fail "timeout"
            Error err -> fail $ "vampire: " <> err
