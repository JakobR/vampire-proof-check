{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module VampireProofCheck.Vampire
  ( runVampire
  , MonadVampire(..)
  , Result(..)
  , UnknownReason(..)
  , VampireStats(..)
  ) where

-- base
import Control.Monad (when)
import Data.List (isPrefixOf)
import System.Exit (ExitCode(..))
import System.IO (stderr, hPutStrLn)

-- clock
import System.Clock (getTime, Clock(Monotonic), diffTimeSpec, toNanoSecs)

-- deepseq
import Control.DeepSeq (deepseq)

-- process
import System.Process (readProcessWithExitCode)

-- safe
import Safe (headDef)

-- time
import Data.Time.Clock (diffTimeToPicoseconds, picosecondsToDiffTime, DiffTime)


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
  { vsRuntime :: !DiffTime  -- ^ how long vampire was running
  }


class Monad m => MonadVampire m where
  runVampire'
    :: Bool            -- ^ whether to print debug output to stderr
    -> FilePath        -- ^ path to vampire executable
    -> Maybe DiffTime  -- ^ vampire timeout, if any
    -> [String]        -- ^ additional command-line options for vampire
    -> String          -- ^ the input to pass to vampire
    -> m (Result, VampireStats, String, String)


instance MonadVampire IO where
  runVampire' = runVampireIO
  {-# INLINABLE runVampire' #-}


runVampire
  :: MonadVampire m
  => FilePath        -- ^ path to vampire executable
  -> Maybe DiffTime  -- ^ vampire timeout, if any
  -> [String]        -- ^ additional command-line options for vampire
  -> String          -- ^ the input to pass to vampire
  -> m (Result, VampireStats, String, String)
runVampire = runVampire' False
{-# INLINABLE runVampire #-}


runVampireIO
  :: Bool
  -> FilePath
  -> Maybe DiffTime
  -> [String]
  -> String
  -> IO (Result, VampireStats, String, String)
runVampireIO debug vampireExe timeout additionalOptions input = do

  let diffTimeToSeconds = (`div` 1_000_000_000_000) . diffTimeToPicoseconds
      mkTimeoutOptions t = [ "--time_limit", show (diffTimeToSeconds t) ]

  let options = [ "--input_syntax", "smtlib2" ]
                ++ maybe [] mkTimeoutOptions timeout
                ++ additionalOptions

  when debug $
    hPutStrLn stderr ("Running vampire with: " <> show options)

  t1 <- (vampireExe, options, input) `deepseq` getTime Monotonic
  r@(exitCode, output, err) <- readProcessWithExitCode vampireExe options input
  t2 <- r `deepseq` getTime Monotonic

  let
    vsRuntime = picosecondsToDiffTime . (*1000) . toNanoSecs $ t2 `diffTimeSpec` t1
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
