{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- base
import Control.Monad ( forM_,  unless )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.List ( intercalate )
import System.IO ( hPrint, stderr )
import System.Exit ( die )

-- containers
-- import Data.Map ( Map )
import qualified Data.Map as Map

-- filepath
import System.FilePath.Posix ( (</>), (<.>) )

-- mtl
import Control.Monad.Reader (runReaderT,  MonadReader(..) )

-- text
import Data.Text ( Text )
import qualified Data.Text as Text
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
    Right proof -> flip runReaderT opts $ checkProof proof

-- | Read all data from file or stdin
readInput :: Maybe FilePath -> IO Text
readInput Nothing = Data.Text.IO.getContents
readInput (Just file) = Data.Text.IO.readFile file

checkProof
  :: (MonadIO m, MonadReader Options m)
  => Proof
  -> m ()
checkProof proof@Proof{..} = do
  forM_ (Map.keys proofStatements) $ \stmtId -> do
    result <- checkStatementId proof stmtId
    case result of
      True -> return ()
      False -> liftIO $ die $ "Check of statement " <> show stmtId <> " failed!"
  liftIO $ putStrLn "Proof is correct!"

checkStatementId
  :: (MonadIO m, MonadReader Options m)
  => Proof -- ^ the proof which is being checked
  -> Id    -- ^ Id of the statement that should be checked
  -> m Bool
checkStatementId Proof{..} checkId =
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
          Options{..} <- ask
          let
            assertExpr :: Expr Text -> Expr Text
            assertExpr e = SExpr [ Value "assert", e ]
            premiseAssertions = assertExpr . unFormula . stmtConclusion <$> premises
            conclusionAssertion = SExpr [ Value "assert-not", unFormula conclusion ]
            exprs :: [Expr Text]
            exprs = (unDecl <$> proofDeclarations)
                    <> premiseAssertions
                    <> [conclusionAssertion]
            vampireInput = intercalate "\n" (showExpr <$> exprs)
            outputBasename = (</> ("inference_" ++ show checkId)) <$> optVampireOutputDir

          forM_ outputBasename $ \basename ->
            liftIO $ writeFile (basename <.> ".in.smt2") vampireInput

          (vampireResult, vampireOutput, vampireError) <-
            runVampire optVampireExe optVampireTimeout vampireInput

          forM_ outputBasename $ \basename -> do
            liftIO $ writeFile (basename <.> ".vout") vampireOutput
            liftIO $ writeFile (basename <.> ".verr") vampireError

          case vampireResult of
            Satisfiable -> return False
            Refutation -> return True
            Timeout -> fail "timeout"
            Error err -> fail $ "vampire: " <> err


showExpr :: Expr Text -> String
showExpr (Value v) = Text.unpack v
showExpr (SExpr xs) = "(" <> intercalate " " (showExpr <$> xs) <> ")"
