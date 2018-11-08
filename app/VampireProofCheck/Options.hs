module VampireProofCheck.Options
  ( Options(..)
  , Seconds
  , execOptionsParser
  ) where

-- optparse-applicative
import Options.Applicative

-- vampire-proof-check
import VampireProofCheck.Types ( Id(..) )


type Seconds = Int

data Options = Options
  { optVampireExe :: FilePath
  , optVampireTimeout :: Seconds
  , optVampireOptions :: String
  , optVampireOutputDir :: Maybe FilePath
  , optProofFile :: Maybe FilePath
  , optCheckOnlyId :: Maybe Id
  , optVerbose :: Bool
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options <$> vampireExe
          <*> vampireTimeout
          <*> vampireOptions
          <*> optional vampireOutputDir
          <*> optional proofFile
          <*> optional checkOnlyId
          <*> verboseFlag
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
    proofFile = argument str (help "Path to the proof file. If not specified, the proof is read from stdin."
                              <> metavar "PROOF-FILE")
    checkOnlyId = Id <$> option auto (long "only"
                                      <> help "Only check the statement with the given id"
                                      <> metavar "ID")
    verboseFlag = switch (short 'v'
                          <> long "verbose"
                          <> help "More output")

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info (optionsParser <**> helper)
       (fullDesc
        <> progDesc "Check correctness of proofs with vampire"
        <> header "vampire-proof-check")

execOptionsParser :: IO Options
execOptionsParser = execParser optionsParserInfo
