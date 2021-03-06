{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module VampireProofCheck.Main
  ( main
  , mainWith
  ) where

-- base
import Control.Concurrent.QSem
import Control.Exception (bracket_, Exception)
import Control.Monad (forM_, when, unless)
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import System.Exit (exitSuccess)
import System.IO (hPrint, hSetBuffering, stderr, stdout, BufferMode(..))
import System.IO.Error (catchIOError, ioError, isDoesNotExistError)

-- async
import Control.Concurrent.Async (forConcurrently)

-- containers
import qualified Data.Map.Strict as Map

-- directory
import System.Directory (createDirectoryIfMissing, removeFile)

-- exceptions
import Control.Monad.Catch (MonadThrow(..))

-- filepath
import System.FilePath.Posix ((</>), (<.>))

-- safe
import Safe (maximumDef)

-- text
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO

-- vampire-proof-check
import qualified Data.Range as Range
import qualified System.Console.ProgressReporter as ProgressReporter
import VampireProofCheck.Options
import VampireProofCheck.Parser (parseProof)
import VampireProofCheck.Types
import VampireProofCheck.Vampire



main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  opts <- execOptionsParser
  mainWith opts


mainWith :: Options -> IO ()
mainWith opts@Options{..} = do
  when optDebug $ hPrint stderr opts

  input <- readInput optProofFile

  proof <-
    liftEitherWith (fatalError . ("unable to parse input:\n"++)) $
    parseProof (fromMaybe "<stdin>" optProofFile) input

  -- Create output directory if it doesn't exist already
  whenJust optVampireOutputDir (createDirectoryIfMissing True)

  numInferences <- checkProof opts proof
  putStrLn $ "Checked " <> showQuantity numInferences "inference" <> ". All of them are correct!"
  exitSuccess


data FatalError = FatalError !String
  deriving Typeable

instance Show FatalError where
  show (FatalError msg) = "Error: " <> msg

instance Exception FatalError

fatalError :: MonadThrow m => String -> m a
fatalError msg = throwM (FatalError msg)


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
  :: Options
  -> Proof
  -> IO Int
checkProof opts@Options{..} proof@Proof{..} = do

  let checkId :: Id -> Bool
      checkId = maybe (const True) (flip Range.member) optCheckOnlyIds

  let idsToCheck = filter checkId (Map.keys proofStatements)

  -- We use a semaphore to limit the number of concurrently executing processes.
  -- (Downside of this solution: creates one thread per statement, and all at the start.
  -- Ideally we'd use a fixed pool of worker threads instead, but it's not a big problem here.)
  workerLimitSem <- newQSem (max 1 optNumWorkers)

  let prCfg = ProgressReporter.Config
        { enabled = optVerbose
        , statusLinePrefix = "Checking statements: "
        , outputHandle = stdout
        }

  checkedStatements <-
    ProgressReporter.withNew prCfg $ \pr ->
      forConcurrently idsToCheck $ \stmtId ->
        bracket_ (waitQSem workerLimitSem) (signalQSem workerLimitSem) $ do
          tok <- ProgressReporter.reportStart pr (show stmtId)

          CheckResult{..} <- checkStatementId opts proof stmtId

          ProgressReporter.reportEnd pr tok $
            "Statement " <> showIdPadded stmtId <> ": "
            <> showResultMark crState
            <> showReason crReason
            <> maybe "" showStats crStats

          case crState of
            StatementTrue ->
              return ()  -- success
            StatementFalse ->
              unless optContinueOnUnproved $
              fatalError $ "statement " <> show stmtId <> " does not hold!"
            StatementTimeout ->
              unless optContinueOnUnproved $
              fatalError $ "while checking statement " <> show stmtId <> ": " <> crReason
            StatementError err ->
              unless optContinueOnError $
              fatalError $ "while checking statement " <> show stmtId <> ": vampire error:" <> err

          return (stmtId, crState)

  let unprovedStatements = fst <$> filter (not . isSuccess . snd) checkedStatements
      idIsInference = maybe False isInference . (proofStatements Map.!?)
      numInferences = length $ filter (idIsInference . fst) checkedStatements
  case unprovedStatements of
    [] -> return numInferences
    _ -> fatalError $ "statements could not be proved: " <> show unprovedStatements

  where
    maxIdLen = maximumDef 0 (length . show <$> Map.keys proofStatements)
    showIdPadded stmtId = let s = show stmtId in replicate (maxIdLen - length s) ' ' <> s
    isSuccess StatementTrue = True
    isSuccess _ = False
    showResultMark StatementTrue = "✓"
    showResultMark StatementFalse = "✗"
    showResultMark _ = "?"
    showReason reason = " [" <> reason <> "]"
    showStats VampireStats{..} = " (vampire: " <> show vsRuntime <> ")"


data StatementState
  = StatementTrue           -- ^ statement was checked successfully and we know that it holds
  | StatementFalse          -- ^ statement was refuted, i.e., we know it is false
  | StatementTimeout        -- ^ timeout/incomplete/etc.; where we continue with --continue-on-timeout
  | StatementError !String  -- ^ error with error message; continue only with --continue-on-error

data CheckResult = CheckResult
  { crState :: !StatementState
  , crReason :: !String  -- ^ optional reason to print in [brackets] after the result mark
  , crStats :: !(Maybe VampireStats)
  }


checkStatementId
  :: Options
  -> Proof -- ^ the proof which is being checked
  -> Id    -- ^ Id of the statement that should be checked
  -> IO CheckResult
checkStatementId opts Proof{..} checkId =
  case Map.lookup checkId proofStatements of
    Nothing ->
      fatalError ("id doesn't appear in proof: " <> show checkId)
    Just (Axiom _ _) ->
      -- Nothing to check for axioms
      return $ CheckResult StatementTrue "axiom" Nothing
    Just (Inference conclusion premises) ->
      let getPremise :: Statement -> (Formula, AxiomType)
          getPremise (Axiom fm t) = (fm, t)
          getPremise (Inference fm _) = (fm, Assumption)
      in checkImplication opts (show checkId) proofDeclarations (getPremise <$> premises) conclusion


checkImplication
  :: Options
  -> String                  -- ^ name for output file
  -> [Declaration]           -- ^ additional declarations
  -> [(Formula, AxiomType)]  -- ^ the premises
  -> Formula                 -- ^ the conclusion
  -> IO CheckResult
checkImplication opts@Options{..} outputName decls premises conclusion = do

  let exprs = exprsForImplicationCheck optAssertNot optAssertTheory decls premises conclusion

  (vampireResult, vampireStats) <- checkExprs opts outputName exprs

  return $ case vampireResult of
    Refutation ->
      CheckResult StatementTrue "proof" (Just vampireStats)
    Satisfiable ->
      CheckResult StatementFalse "sat" (Just vampireStats)
    Unknown Timeout ->
      CheckResult StatementTimeout "timeout" (Just vampireStats)
    Unknown IncompleteStrategy ->
      CheckResult StatementTimeout "incomplete strategy" (Just vampireStats)
    Unknown (Error msg) ->
      CheckResult (StatementError msg) "error" (Just vampireStats)


checkExprs
  :: Options
  -> String         -- ^ base for output file names
  -> [Expr Text]    -- ^ expressions to check with vampire
  -> IO (Result, VampireStats)
checkExprs Options{..} outputName exprs = do

  let vampireInput = unlines (showExpr <$> exprs)
      outputBasename = (</> outputName) <$> optVampireOutputDir
      additionalOptions = words optVampireOptions

  forM_ outputBasename $ \basename ->
    writeFile (basename <.> ".in.smt2") vampireInput

  (vampireResult, vampireStats, vampireOutput, vampireError) <-
    runVampire' optDebug optVampireExe optVampireTimeout additionalOptions vampireInput

  forM_ outputBasename $ \basename -> do
    writeFile (basename <.> ".vout") vampireOutput
    writeFileUnlessEmpty (basename <.> ".verr") vampireError

  return (vampireResult, vampireStats)


assertPremise :: OptAssertTheory -> (Expr Text, AxiomType) -> Expr Text
assertPremise opt (expr, TheoryAxiom) = assertTheoryExpr opt expr
assertPremise _ (expr, Assumption) = assertExpr expr


assertExpr :: Expr Text -> Expr Text
assertExpr e = SExpr [ Value "assert", e ]


assertTheoryExpr :: OptAssertTheory -> Expr Text -> Expr Text
assertTheoryExpr UseAssertTheory expr = SExpr [ Value "assert-theory", expr ]
assertTheoryExpr AvoidAssertTheory expr = assertExpr expr


assertNotExpr :: OptAssertNot -> Expr Text -> Expr Text
assertNotExpr AvoidAssertNot expr = SExpr [ Value "assert", SExpr [ Value "not", expr ] ]
assertNotExpr UseAssertNot expr = SExpr [ Value "assert-not", expr ]


exprsForImplicationCheck
  :: OptAssertNot            -- ^ whether to avoid assert-not statements
  -> OptAssertTheory         -- ^ whether to avoid assert-theory statements
  -> [Declaration]           -- ^ declarations
  -> [(Formula, AxiomType)]  -- ^ premises
  -> Formula                 -- ^ conclusion
  -> [Expr Text]             -- ^ SMT-LIB 2 expressions for implication check
exprsForImplicationCheck optAssertNot optAssertTheory decls premises conclusion =
  (unDecl <$> decls)
  <> (assertPremise optAssertTheory . first unFormula <$> premises)
  <> [assertNotExpr optAssertNot . unFormula $ conclusion]


showExpr :: Expr Text -> String
showExpr (Value v) = Text.unpack v
showExpr (SExpr xs) = "(" <> intercalate " " (showExpr <$> xs) <> ")"


-- withErrorPrefix
--   :: MonadError String m
--   => String
--   -> m a
--   -> m a
-- withErrorPrefix prefix m =
--   m `catchError` \err -> throwError (prefix <> ": " <> err)


liftEitherWith :: Monad m => (e -> m a) -> Either e a -> m a
liftEitherWith _ (Right x) = return x
liftEitherWith f (Left e) = f e


-- | If content isn't empty, write it to the given file, otherwise delete the file.
writeFileUnlessEmpty :: FilePath -> String -> IO ()
writeFileUnlessEmpty file content
  | isEmpty content = removeFile file `catchIOError` ignoreDoesNotExistError
  | otherwise       = writeFile file content
  where
    isEmpty = all isSpace
    ignoreDoesNotExistError e | isDoesNotExistError e = return ()
                              | otherwise             = ioError e
