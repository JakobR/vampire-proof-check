{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- base
import Control.Monad ( forM_,  unless )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.List ( intercalate )
import System.Exit ( die, exitSuccess )

-- containers
import qualified Data.Map.Strict as Map

-- filepath
import System.FilePath.Posix ( (</>), (<.>) )

-- mtl
import Control.Monad.Except ( runExceptT,  MonadError(..) )
import Control.Monad.Reader ( runReaderT,  MonadReader(..) )

-- text
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.IO

-- vampire-proof-check
import VampireProofCheck.Options
import VampireProofCheck.Parser ( parseProof )
import VampireProofCheck.Types
import VampireProofCheck.Vampire ( runVampire, VampireResult(..) )




main :: IO ()
main = do
  opts@Options{..} <- execOptionsParser
  input <- readInput optProofFile
  result <- runExceptT $ do
    proof <- withErrorPrefix "unable to parse input" $ parseProof input
    flip runReaderT opts $ checkProof proof
    return $ length . filter isInference . map snd . Map.toList . proofStatements $ proof
  case result of
    Left err ->
      die $ "Error: " <> err
    Right numInferences -> do
      putStrLn $ "Checked " <> show numInferences <> " inferences. Proof is correct!"
      exitSuccess

-- | Read all data from file or stdin
readInput :: Maybe FilePath -> IO Text
readInput Nothing = Data.Text.IO.getContents
readInput (Just file) = Data.Text.IO.readFile file

checkProof
  :: (MonadIO m, MonadReader Options m, MonadError String m)
  => Proof
  -> m ()
checkProof proof@Proof{..} = do
  forM_ (Map.keys proofStatements) $ \stmtId -> do
    result <-
      withErrorPrefix ("while checking statement " <> show stmtId) $ checkStatementId proof stmtId
    case result of
      False -> throwError $ "statement " <> show stmtId <> " does not hold!"
      True -> return ()

checkStatementId
  :: (MonadIO m, MonadReader Options m, MonadError String m)
  => Proof -- ^ the proof which is being checked
  -> Id    -- ^ Id of the statement that should be checked
  -> m Bool
checkStatementId Proof{..} checkId =
  case Map.lookup checkId proofStatements of
    Nothing ->
      throwError $ "id doesn't appear in proof: " <> show checkId
    Just (Axiom _) ->
      -- Nothing to check for axioms
      return True
    Just (Inference conclusion premiseIds) -> do
      -- Inference may only depend on earlier statements
      unless (all (< checkId) premiseIds) $
        throwError $ "inference depends on subsequent formula: " <> show checkId
      -- Look up premises and fail if one doesn't exist
      let
        lookupId :: Id -> Either Id Statement
        lookupId theId = maybe (Left theId) Right (Map.lookup theId proofStatements)

      case sequenceA (lookupId <$> premiseIds) of
        Left errId ->
          throwError $ "inference depends on non-existing premise " <> show errId
        Right premises ->
          checkImplication (show checkId) proofDeclarations (stmtConclusion <$> premises) conclusion

checkImplication
  :: (MonadIO m, MonadReader Options m, MonadError String m)
  => String         -- ^ name for output file
  -> [Declaration]  -- ^ additional declarations
  -> [Formula]      -- ^ the premises
  -> Formula        -- ^ the conclusion
  -> m Bool
checkImplication outputName decls premises conclusion = do
  Options{..} <- ask
  let
    assertExpr :: Expr Text -> Expr Text
    assertExpr e = SExpr [ Value "assert", e ]
    premiseAssertions = assertExpr . unFormula <$> premises
    conclusionAssertion = SExpr [ Value "assert-not", unFormula conclusion ]
    exprs :: [Expr Text]
    exprs = (unDecl <$> decls)
            <> premiseAssertions
            <> [conclusionAssertion]
    vampireInput = intercalate "\n" (showExpr <$> exprs)
    outputBasename = (</> ("inference_" ++ outputName)) <$> optVampireOutputDir

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
    Timeout -> throwError "timeout"
    Error err -> throwError $ "vampire: " <> err


showExpr :: Expr Text -> String
showExpr (Value v) = Text.unpack v
showExpr (SExpr xs) = "(" <> intercalate " " (showExpr <$> xs) <> ")"


withErrorPrefix
  :: MonadError String m
  => String
  -> m a
  -> m a
withErrorPrefix prefix m =
  m `catchError` \err -> throwError (prefix <> ": " <> err)
