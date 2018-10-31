{-# LANGUAGE ScopedTypeVariables #-}

module VampireProofChecker.Options
  ( Options(..)
  , Seconds
  , execOptionsParser
  ) where

-- optparse-applicative
import Options.Applicative


type Seconds = Int

data Options = Options
  { optVampireExe :: FilePath
  , optVampireTimeout :: Seconds
  , optProofFile :: Maybe FilePath
  }
  deriving (Show)

optionsParser :: Parser Options
optionsParser =
  Options <$> vampireExe
          <*> vampireTimeout
          <*> optional proofFile
  where
    vampireExe = strOption (long "vampire-exe"
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
    proofFile = argument str (help "Path to the proof file. If not specified, read from stdin."
                              <> metavar "PROOF-FILE")

optionsParserInfo :: ParserInfo Options
optionsParserInfo =
  info (optionsParser <**> helper)
       (fullDesc
        <> progDesc "Check correctness of proofs with vampire"
        <> header "vampire-proof-check")

execOptionsParser :: IO Options
execOptionsParser = execParser optionsParserInfo
