{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

-- base
import Control.Monad ( forM, forM_, when )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.List ( intercalate )
import System.Exit ( die, exitSuccess )
import System.IO ( hSetBuffering, stdout, BufferMode(..) )

-- containers
import qualified Data.Map.Strict as Map

-- filepath
import System.FilePath.Posix ( (</>), (<.>) )

-- mtl
import Control.Monad.Except ( runExceptT,  MonadError(..) )
import Control.Monad.Reader ( asks, runReaderT,  MonadReader(..) )

-- safe
import Safe ( maximumDef )

-- text
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.IO

-- vampire-proof-check
import VampireProofCheck.Options
import VampireProofCheck.Parser ( parseProof )
import VampireProofCheck.Types
import VampireProofCheck.Vampire ( runVampire, VampireResult(..), VampireStats(..) )



main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  opts@Options{..} <- execOptionsParser
  input <- readInput optProofFile
  result <- runExceptT $ do
    proof <- withErrorPrefix "unable to parse input" $ parseProof input
    runReaderT (checkProof proof) opts
  case result of
    Left err ->
      die $ "Error: " <> err
    Right numInferences -> do
      putStrLn $ "Checked " <> showQuantity numInferences "inference" <> ". Proof is correct!"
      exitSuccess


-- | Read all data from file or stdin
readInput :: Maybe FilePath -> IO Text
readInput Nothing = Data.Text.IO.getContents
readInput (Just file) = Data.Text.IO.readFile file


showQuantity :: (Eq a, Num a, Show a) => a -> String -> String
showQuantity 1 name = "1 " <> name
showQuantity n name = show n <> " " <> name <> "s"


checkProof
  :: (MonadIO m, MonadReader Options m, MonadError String m)
  => Proof
  -> m Int
checkProof proof@Proof{..} = do
  idsToCheck <- maybe (Map.keys proofStatements) (:[]) <$> asks optCheckOnlyId

  checkedInferences <- forM idsToCheck $ \stmtId -> do
    verbose <- asks optVerbose
    when verbose $ liftIO $
      putStr $ "Checking statement " <> showIdPadded stmtId <> "... "

    (result, mstats) <-
      withErrorPrefix ("while checking statement " <> show stmtId) $ checkStatementId proof stmtId

    when verbose $ liftIO $
      putStrLn $ (if result then "✓" else "✗") <> maybe "" showStats mstats

    case result of
      False -> throwError $ "statement " <> show stmtId <> " does not hold!"
      True -> return $ maybe False isInference (Map.lookup stmtId proofStatements)

  return . length . filter (==True) $ checkedInferences

  where
    maxIdLen = maximumDef 0 (length . show <$> Map.keys proofStatements)
    showIdPadded stmtId = let s = show stmtId in replicate (maxIdLen - length s) ' ' <> s
    showStats VampireStats{..} = " (vampire: " <> show vsRuntime <> ")"


checkStatementId
  :: (MonadIO m, MonadReader Options m, MonadError String m)
  => Proof -- ^ the proof which is being checked
  -> Id    -- ^ Id of the statement that should be checked
  -> m (Bool, Maybe VampireStats)
checkStatementId Proof{..} checkId =
  case Map.lookup checkId proofStatements of
    Nothing ->
      throwError $ "id doesn't appear in proof: " <> show checkId
    Just (Axiom _) ->
      -- Nothing to check for axioms
      return (True, Nothing)
    Just (Inference conclusion premiseIds) -> do
      -- Inference may only depend on earlier statements
      case filter (>= checkId) premiseIds of
        [] -> return ()
        xs -> throwError $ "inference may only depends on earlier formulas, but depends on "
                           <> intercalate ", " (show <$> xs)

      -- Look up premises and fail if one doesn't exist
      let
        lookupId :: Id -> Either Id Statement
        lookupId theId = maybe (Left theId) Right (Map.lookup theId proofStatements)

      case sequenceA (lookupId <$> premiseIds) of
        Left errId ->
          throwError $ "inference depends on non-existing premise " <> show errId
        Right premises -> do
          (r, s) <- checkImplication (show checkId) proofDeclarations (stmtConclusion <$> premises) conclusion
          return (r, Just s)


checkImplication
  :: (MonadIO m, MonadReader Options m, MonadError String m)
  => String         -- ^ name for output file
  -> [Declaration]  -- ^ additional declarations
  -> [Formula]      -- ^ the premises
  -> Formula        -- ^ the conclusion
  -> m (Bool, VampireStats)
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
    -- TODO: should we allow some mechanism to pass options with spaces to vampire?
    additionalOptions = words optVampireOptions

  forM_ outputBasename $ \basename ->
    liftIO $ writeFile (basename <.> ".in.smt2") vampireInput

  (vampireResult, vampireStats, vampireOutput, vampireError) <-
    runVampire optVampireExe optVampireTimeout additionalOptions vampireInput

  forM_ outputBasename $ \basename -> do
    liftIO $ writeFile (basename <.> ".vout") vampireOutput
    liftIO $ writeFile (basename <.> ".verr") vampireError

  case vampireResult of
    Satisfiable -> return (False, vampireStats)
    Refutation -> return (True, vampireStats)
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
