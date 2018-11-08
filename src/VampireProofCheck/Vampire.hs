{-# LANGUAGE RecordWildCards #-}

module VampireProofCheck.Vampire
  ( runVampire
  , Seconds
  , VampireResult(..)
  , VampireStats(..)
  ) where

-- base
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.List ( isPrefixOf )
import System.Exit ( ExitCode(..) )

-- deepseq
import Control.DeepSeq ( deepseq )

-- process
import System.Process ( readProcessWithExitCode )

-- time
import Data.Time.Clock ( NominalDiffTime, diffUTCTime )
import Data.Time.Clock.System ( getSystemTime, systemToUTCTime )


-- NOTE: Maybe we should use NominalDiffTime instead of a naked integer?
type Seconds = Int

data VampireStats = VampireStats
  { vsRuntime :: !NominalDiffTime  -- ^ how long vampire was running
  }

runVampire
  :: MonadIO m
  => FilePath  -- ^ path to vampire executable
  -> Seconds   -- ^ vampire timeout (0 means no limit)
  -> [String]  -- ^ additional command-line options for vampire
  -> String    -- ^ the input to pass to vampire
  -> m (VampireResult, VampireStats, String, String)
runVampire vampireExe timeoutSecs additionalOptions input = do
  let options = [ "--input_syntax", "smtlib2"
                , "--time_limit", show timeoutSecs
                ] ++ additionalOptions

  t1 <- (vampireExe, options, input) `deepseq` liftIO getSystemTime
  r@(exitCode, output, err) <- liftIO $ readProcessWithExitCode vampireExe options input
  t2 <- r `deepseq` liftIO getSystemTime

  let
    vsRuntime = systemToUTCTime t2 `diffUTCTime` systemToUTCTime t1
    vampireResult = parseVampireOutput exitCode output

  return $ (vampireResult, VampireStats{..}, output, err)

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
  | Error !String
  deriving (Eq, Show)
