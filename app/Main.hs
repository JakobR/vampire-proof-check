{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

-- base
import Control.Monad (forM, forM_, when, unless)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (isSpace)
import Data.List (intercalate)
import System.Exit (die, exitSuccess)
import System.IO (hPrint, hSetBuffering, stderr, stdout, BufferMode(..))
import System.IO.Error (catchIOError, ioError, isDoesNotExistError)

-- containers
import qualified Data.Map.Strict as Map

-- directory
import System.Directory (createDirectoryIfMissing, removeFile)

-- filepath
import System.FilePath.Posix ((</>), (<.>))

-- mtl
import Control.Monad.Except (runExceptT,  MonadError(..))
import Control.Monad.Reader (asks, runReaderT,  MonadReader(..))

-- safe
import Safe (maximumDef)

-- text
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO

-- vampire-proof-check
import qualified Data.Range as Range
import VampireProofCheck.Options
import VampireProofCheck.Parser (parseProof)
import VampireProofCheck.Types
import VampireProofCheck.Vampire



main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  opts@Options{..} <- execOptionsParser
  when optDebug $ hPrint stderr opts
  input <- readInput optProofFile

  result <- runExceptT $ do
    proof <- withErrorPrefix "unable to parse input" $ parseProof optProofFile input

    -- Create output directory if it doesn't exist already
    whenJust optVampireOutputDir (liftIO . createDirectoryIfMissing True)

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


whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust = flip $ maybe (pure ())


showQuantity :: (Eq a, Num a, Show a) => a -> String -> String
showQuantity 1 name = "1 " <> name
showQuantity n name = show n <> " " <> name <> "s"


checkProof
  :: (MonadIO m, MonadReader Options m, MonadError String m)
  => Proof
  -> m Int
checkProof proof@Proof{..} = do
  checkId <- maybe (const True) (flip Range.member) <$> asks optCheckOnlyIds
  let idsToCheck = filter checkId (Map.keys proofStatements)

  checkedStatements <- forM idsToCheck $ \stmtId -> do
    verbose <- asks optVerbose
    when verbose $ liftIO $
      putStr $ "Checking statement " <> showIdPadded stmtId <> "... "

    (result, mVampire) <-
      withErrorPrefix ("while checking statement " <> show stmtId) $ checkStatementId proof stmtId

    when verbose $ liftIO $
      putStrLn $ showResult result <> maybe "" showStats mVampire

    continueOnError <- asks optContinueOnError
    unless continueOnError $
      case result of
        Right True ->
          return ()  -- success
        Right False ->
          throwError $ "statement " <> show stmtId <> " does not hold!"
        Left u ->
          let msg = case u of Timeout -> "timeout"
                              IncompleteStrategy -> "incomplete strategy"
                              Error err -> "vampire: " <> err
          in throwError $ "while checking statement " <> show stmtId <> ": " <> msg

    return (stmtId, result)

  let falseStatements = fst <$> filter (not . isSuccess . snd) checkedStatements
      idIsInference = maybe False isInference . (proofStatements Map.!?)
      numInferences = length $ filter (idIsInference . fst) checkedStatements
  case falseStatements of
    [] -> return numInferences
    _ -> throwError $ "statements do not hold: " <> show falseStatements

  where
    maxIdLen = maximumDef 0 (length . show <$> Map.keys proofStatements)
    showIdPadded stmtId = let s = show stmtId in replicate (maxIdLen - length s) ' ' <> s
    isSuccess (Right b) = b
    isSuccess (Left _) = False
    showResult (Right True) = "✓"
    showResult (Right False) = "✗"
    showResult (Left u) = let reason = case u of Timeout -> "timeout"
                                                 IncompleteStrategy -> "incomplete strategy"
                                                 Error _ -> "error"
                          in "✗ [" <> reason <> "]"
    showStats VampireStats{..} = " (vampire: " <> show vsRuntime <> ")"


checkStatementId
  :: (MonadIO m, MonadReader Options m, MonadError String m)
  => Proof -- ^ the proof which is being checked
  -> Id    -- ^ Id of the statement that should be checked
  -> m (Either UnknownReason Bool, Maybe VampireStats)
checkStatementId Proof{..} checkId =
  case Map.lookup checkId proofStatements of
    Nothing ->
      throwError $ "id doesn't appear in proof: " <> show checkId
    Just (Axiom _) ->
      -- Nothing to check for axioms
      return (Right True, Nothing)
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
  :: (MonadIO m, MonadReader Options m)
  => String         -- ^ name for output file
  -> [Declaration]  -- ^ additional declarations
  -> [Formula]      -- ^ the premises
  -> Formula        -- ^ the conclusion
  -> m (Either UnknownReason Bool, VampireStats)
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
    additionalOptions = words optVampireOptions

  forM_ outputBasename $ \basename -> liftIO $
    writeFile (basename <.> ".in.smt2") vampireInput

  (vampireResult, vampireStats, vampireOutput, vampireError) <- liftIO $
    runVampire' optDebug optVampireExe optVampireTimeout additionalOptions vampireInput

  forM_ outputBasename $ \basename -> liftIO $ do
    writeFile (basename <.> ".vout") vampireOutput
    writeFileUnlessEmpty (basename <.> ".verr") vampireError

  let checkResult = case vampireResult of
                      Satisfiable -> Right False
                      Refutation -> Right True
                      Unknown u -> Left u
  return (checkResult, vampireStats)


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


-- | If content isn't empty, write it to the given file, otherwise delete the file.
writeFileUnlessEmpty :: FilePath -> String -> IO ()
writeFileUnlessEmpty file content
  | isEmpty content = removeFile file `catchIOError` ignoreDoesNotExistError
  | otherwise       = writeFile file content
  where
    isEmpty = all isSpace
    ignoreDoesNotExistError e | isDoesNotExistError e = return ()
                              | otherwise             = ioError e
