{-# LANGUAGE RecordWildCards #-}

module VampireProofCheck.Vampire
  ( runVampire
  , Seconds
  , Result(..)
  , UnknownReason(..)
  , VampireStats(..)
  ) where

-- base
import Control.Monad.IO.Class (MonadIO(..))
import Data.List (isPrefixOf)
import System.Exit (ExitCode(..))

-- deepseq
import Control.DeepSeq (deepseq)

-- process
import System.Process (readProcessWithExitCode)

-- safe
import Safe (headDef)

-- time
import Data.Time.Clock (NominalDiffTime, diffUTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)


-- NOTE: Maybe we should use NominalDiffTime instead of a naked integer?
type Seconds = Int

data Result
  = Satisfiable
  | Refutation
  | Unknown !UnknownReason
  deriving (Eq, Show)

data UnknownReason
  = Timeout
  | IncompleteStrategy
  | Error !String
  deriving (Eq, Show)

data VampireStats = VampireStats
  { vsRuntime :: !NominalDiffTime  -- ^ how long vampire was running
  }


runVampire
  :: MonadIO m
  => FilePath  -- ^ path to vampire executable
  -> Seconds   -- ^ vampire timeout (0 means no limit)
  -> [String]  -- ^ additional command-line options for vampire
  -> String    -- ^ the input to pass to vampire
  -> m (Result, VampireStats, String, String)
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


parseVampireOutput :: ExitCode -> String -> Result
parseVampireOutput exitCode output =
  case (exitCode, primaryReasons, secondaryReasons) of

    (ExitSuccess, ["Theorem"], _) ->
      Refutation

    (ExitSuccess, ["Unsatisfiable"], _) ->
      Refutation

    (ExitSuccess, ["CounterSatisfiable"], _) ->
      Satisfiable

    (_, ["Timeout"], _) ->
      Unknown Timeout

    (_, [], ["Time limit"]) ->
      Unknown Timeout

    (_, [], ["Refutation not found, incomplete strategy"]) ->
      Unknown IncompleteStrategy

    (_, _, _) ->
      let msg = "ExitCode = " <> show exitCode
                <> "; Unknown termination reason: " <> show allReasons
                <> "\nOutput:\n" <> output
      in Unknown (Error msg)

  where
    primaryReasons = headDef "<none>" . words <$> extractTerminationReasons "% SZS status " output
    secondaryReasons = extractTerminationReasons "% Termination reason: " output
    allReasons = (primaryReasons, secondaryReasons)


extractTerminationReasons :: String -> String -> [String]
extractTerminationReasons reasonPrefix output = extractReason <$> reasonLines
  where
    reasonLines = filter (reasonPrefix `isPrefixOf`) (lines output)
    extractReason = drop (length reasonPrefix)
