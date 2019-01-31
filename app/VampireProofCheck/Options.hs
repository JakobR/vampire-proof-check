module VampireProofCheck.Options
  ( OptAssertNot(..)
  , Options(..)
  , execOptionsParser
  ) where

-- base
import GHC.Conc (getNumProcessors)

-- optparse-applicative
import Options.Applicative

-- time
import Data.Time.Clock (DiffTime)

-- vampire-proof-check
import Data.Range (parseIntegerRange', Range(..))
import VampireProofCheck.Types (Id(..))


data OptAssertNot = AvoidAssertNot | UseAssertNot
  deriving Show


data Options = Options
  { optVampireExe :: FilePath
  , optVampireTimeout :: Maybe DiffTime
  , optVampireOptions :: String
  , optVampireOutputDir :: Maybe FilePath
  , optCheckOnlyIds :: Maybe (Range Id)
  , optContinueOnUnproved :: Bool
  , optContinueOnError :: Bool
  , optNumWorkers :: Int
  , optVerbose :: Bool
  , optAssertNot :: OptAssertNot
  , optProofFile :: Maybe FilePath
  , optDebug :: Bool
  }
  deriving Show


optionsParser :: Int -> Parser Options
optionsParser numProcessors =
  pure Options
  <*> vampireExe
  <*> optional vampireTimeout
  <*> vampireOptions
  <*> optional vampireOutputDir
  <*> optional checkOnlyIds
  <*> continueOnUnprovedFlag
  <*> continueOnErrorFlag
  <*> numWorkers
  <*> verboseFlag
  <*> assertNot
  <*> optional proofFile
  <*> debugFlag

  where

    vampireExe =
      strOption $
      short 'x'
      <> long "vampire-exe"
      <> help "Path to vampire executable"
      <> value "vampire"
      <> showDefault
      <> metavar "PATH"

    vampireTimeout =
      option (fromInteger <$> auto) $
      short 't'
      <> long "vampire-timeout"
      <> help "Timeout for vampire in seconds"
      <> metavar "SECONDS"

    vampireOptions =
      strOption $
      short 'o'
      <> long "vampire-options"
      <> help "Additional options that should be passed to vampire"
      <> value ""
      <> metavar "OPTIONS"

    vampireOutputDir =
      strOption $
      long "vampire-output-dir"
      <> help "Path to directory where vampire output should be stored"
      <> metavar "PATH"

    checkOnlyIds =
      option idRange $
      long "only"
      <> help ("Only check the statements in the given range of ids "
               ++ "(for example: 1,2,5-10)")
      <> metavar "IDs"

    continueOnUnprovedFlag =
      switch $
      short 'c'
      <> long "continue-on-unproved"
      <> help ("When a proof step cannot be proved due to a timeout "
               ++ "or because it does not hold, "
               ++ "continue checking the subsequent inferences.")

    continueOnErrorFlag =
      switch $
      long "continue-on-error"
      <> help ("When a proof step fails due to a vampire error, "
               ++ "continue checking the subsequent inferences.")

    numWorkers =
      option auto $
      short 'n'
      <> long "num-workers"
      <> value numProcessors
      <> showDefault
      <> help "How many vampire instances to run in parallel"

    verboseFlag =
      switch $
      short 'v'
      <> long "verbose"
      <> help "More output"

    assertNot =
      flag UseAssertNot AvoidAssertNot $
      long "avoid-assert-not"
      <> help ("Use (assert (not ...)) instead of (assert-not ...). "
               ++ "Useful for vampire versions without smtlib_extras.")

    proofFile =
      argument str $
      help "Path to the proof file. If not specified, the proof is read from stdin."
      <> metavar "PROOF-FILE"

    debugFlag =
      switch $
      long "debug"
      <> help "Print some debug output"


optionsParserInfo :: Int -> ParserInfo Options
optionsParserInfo numProcessors =
  info (optionsParser numProcessors <**> helper)
       (fullDesc
        <> progDesc "Check correctness of proofs with vampire"
        <> header "vampire-proof-check")


execOptionsParser :: IO Options
execOptionsParser = do
  -- NOTE: we want processors and not capabilities, since vampire runs as a separate process
  numProcessors <- max 1 <$> getNumProcessors
  execParser (optionsParserInfo numProcessors)


idRange :: ReadM (Range Id)
idRange = eitherReader (parseIntegerRange' Id)
