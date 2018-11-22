module VampireProofCheck.Options
  ( Options(..)
  , Seconds
  , execOptionsParser
  ) where

-- optparse-applicative
import Options.Applicative

-- vampire-proof-check
import Data.Range ( parseIntegerRange', Range(..) )
import VampireProofCheck.Types ( Id(..) )


type Seconds = Int

data Options = Options
  { optVampireExe :: FilePath
  , optVampireTimeout :: Seconds
  , optVampireOptions :: String
  , optVampireOutputDir :: Maybe FilePath
  , optCheckOnlyIds :: Maybe (Range Id)
  , optVerbose :: Bool
  , optProofFile :: Maybe FilePath
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options <$> vampireExe
          <*> vampireTimeout
          <*> vampireOptions
          <*> optional vampireOutputDir
          <*> optional checkOnlyIds
          <*> verboseFlag
          <*> optional proofFile
  where
    vampireExe = strOption (short 'x'
                            <> long "vampire-exe"
                            <> help "Path to vampire executable"
                            <> value "vampire"
                            <> showDefault
                            <> metavar "PATH")
    vampireTimeout = option auto (short 't'
                                  <> long "vampire-timeout"
                                  <> help "Timeout for vampire in seconds, 0 means no limit"
                                  <> value 5
                                  <> showDefault
                                  <> metavar "SECONDS")
    vampireOptions = strOption (short 'o'
                                <> long "vampire-options"
                                <> help "Additional options that should be passed to vampire"
                                <> value ""
                                <> showDefault
                                <> metavar "OPTIONS")
    vampireOutputDir = strOption (long "vampire-output-dir"
                                  <> help "Path to directory where vampire output should be stored"
                                  <> metavar "PATH")
    checkOnlyIds = option idRange (long "only"
                                   <> help ("Only check the statements in the given range of ids "
                                            ++ "(for example: 1,2,5-10)")
                                   <> metavar "ID")
    verboseFlag = switch (short 'v'
                          <> long "verbose"
                          <> help "More output")
    proofFile = argument str (help "Path to the proof file. If not specified, the proof is read from stdin."
                              <> metavar "PROOF-FILE")

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
