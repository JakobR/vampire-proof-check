module VampireProofCheck.Options
  ( Options(..)
  , execOptionsParser
  ) where

-- optparse-applicative
import Options.Applicative

-- time
import Data.Time.Clock (DiffTime)

-- vampire-proof-check
import Data.Range (parseIntegerRange', Range(..))
import VampireProofCheck.Types (Id(..))


data Options = Options
  { optVampireExe :: FilePath
  , optVampireTimeout :: Maybe DiffTime
  , optVampireOptions :: String
  , optVampireOutputDir :: Maybe FilePath
  , optCheckOnlyIds :: Maybe (Range Id)
  , optContinueOnError :: Bool
  , optVerbose :: Bool
  , optNoAssertNot :: Bool
  , optProofFile :: Maybe FilePath
  , optDebug :: Bool
  }
  deriving (Show)


optionsParser :: Parser Options
optionsParser =
  pure Options
  <*> vampireExe
  <*> optional vampireTimeout
  <*> vampireOptions
  <*> optional vampireOutputDir
  <*> optional checkOnlyIds
  <*> continueOnErrorFlag
  <*> verboseFlag
  <*> noAssertNotFlag
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
      <> metavar "ID"

    continueOnErrorFlag =
      switch $
      short 'e'
      <> long "continue-on-error"
      <> help "When a proof step fails, continue checking the subsequent inferences."

    verboseFlag =
      switch $
      short 'v'
      <> long "verbose"
      <> help "More output"

    noAssertNotFlag =
      switch $
      long "no-assert-not"
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


optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info (optionsParser <**> helper)
       (fullDesc
        <> progDesc "Check correctness of proofs with vampire"
        <> header "vampire-proof-check")


execOptionsParser :: IO Options
execOptionsParser = execParser optionsParserInfo


idRange :: ReadM (Range Id)
idRange = eitherReader (parseIntegerRange' Id)
