{-# LANGUAGE ScopedTypeVariables #-}

module VampireProofChecker.Vampire
  ( runVampire
  , Seconds
  , VampireResult(..)
  ) where

-- base
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.List ( isPrefixOf )
import System.Exit ( ExitCode(..) )

-- process
import System.Process ( readProcessWithExitCode )


type Seconds = Int

runVampire
  :: MonadIO m
  => FilePath  -- ^ path to vampire executable
  -> Seconds   -- ^ vampire timeout (0 means no limit)
  -> String    -- ^ the input to pass to vampire
  -> m (VampireResult, String, String)
runVampire vampireExe timeoutSecs input = do
  let options = [ "--input_syntax", "smtlib2"
                -- , "--proof", "off"
                , "--avatar", "off"
                -- , "--mode", "portfolio"
                -- , "--statistics", "none"
                , "--time_limit", show timeoutSecs]
  (exitCode, output, err) <- liftIO $ readProcessWithExitCode vampireExe options input
  let vampireResult = parseVampireOutput exitCode output
  return $ (vampireResult, output, err)

parseVampireOutput :: ExitCode -> String -> VampireResult
parseVampireOutput exitCode output =
  case (exitCode, reasons) of
    (ExitSuccess, ["Theorem"]) -> Refutation
    (ExitSuccess, ["Unsatisfiable"]) -> Refutation
    (ExitSuccess, ["CounterSatisfiable"]) -> Satisfiable
    (_, ["Timeout"]) -> Timeout
    (_, _) -> Error ("ExitCode = " <> show exitCode
                      <> "; Unknown termination reason: " <> show reasons
                      <> "\nOutput:\n" <> output)
  where
    reasonPrefix = "% SZS status "
    extractReason = head . (++["<none>"]) . take 1 . words . drop (length reasonPrefix)
    reasonLines = filter (reasonPrefix `isPrefixOf`) (lines output)
    reasons = extractReason <$> reasonLines

data VampireResult
  = Satisfiable
  | Refutation
  | Timeout
  | Error String
  deriving (Eq, Show)
