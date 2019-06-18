{-# LANGUAGE EmptyDataDeriving #-}

module Vampire.Benchmark.Options
  ( parseOptions
  , Options
  , Options'(..)
  , Command
  , Command'(..)
  , RunOptions
  , RunOptions'(..)
  , AnalyseOptions
  ) where

-- base
import GHC.Conc (getNumProcessors)

-- optparse-applicative
import Options.Applicative

-- path
import Path

-- path-io
import Path.IO

-- time
import Data.Time.Clock (secondsToDiffTime, DiffTime)


type Options = Options' (Path Abs File) (Path Abs Dir)
type UnresolvedOptions = Options' FilePath FilePath

type Command = Command' (Path Abs File) (Path Abs Dir)
type UnresolvedCommand = Command' FilePath FilePath

type RunOptions = RunOptions' (Path Abs File) (Path Abs Dir)
type UnresolvedRunOptions = RunOptions' FilePath FilePath

data Options' file dir = Options
  { optCommand :: !(Command' file dir)
  , optDebug :: !Bool
  , optVerbose :: !Bool
  }
  deriving Show

data Command' file dir
  = Run !(RunOptions' file dir)
  | Analyse !AnalyseOptions
  deriving Show

data RunOptions' file dir = RunOptions
  { rOptDataDir :: !dir
  , rOptProblemRoot :: !dir
    -- ^ TODO: support single files too? (=> problem may be a file or directory)
  , rOptVampireExe :: !FilePath
  , rOptVampireTimeout :: !DiffTime
  , rOptVampireOptions :: ![String]
  , rOptVampireWorkDir :: !dir
  , rOptProblemFilenameGlobs :: ![String]
    -- ^ if not empty, only process filenames match at least one of the glob patterns
  , rOptProblemContentFilters :: ![String]
    -- ^ process only problems whose content matches all of the given patterns (regexes)
  , rOptNumWorkers :: !Int
    -- ^ number of vampire instances to run in parallel
  , rOptForce :: !Bool
    -- ^ continue despite warnings
  }
  deriving Show

-- | Not yet implemented (TODO)
data AnalyseOptions
  deriving Show


resolveOptions :: UnresolvedOptions -> IO Options
resolveOptions o = do
  resolvedCommand <- resolveCommand (optCommand o)
  return Options{ optCommand = resolvedCommand
                , optDebug = optDebug o
                , optVerbose = optVerbose o
                }


resolveCommand :: UnresolvedCommand -> IO Command
resolveCommand (Run o) = Run <$> resolveRunOptions o
resolveCommand (Analyse o) = return $ Analyse o


resolveRunOptions :: UnresolvedRunOptions -> IO RunOptions
resolveRunOptions o = do
  resolvedDataDir <-
    resolveDir' (rOptDataDir o)

  resolvedProblemRoot <-
    resolveDir' (rOptProblemRoot o)

  resolvedVampireWorkDir <-
    resolveDir' (rOptVampireWorkDir o)

  return RunOptions{ rOptDataDir = resolvedDataDir
                   , rOptProblemRoot = resolvedProblemRoot
                   , rOptVampireExe = rOptVampireExe o
                   , rOptVampireTimeout = rOptVampireTimeout o
                   , rOptVampireOptions = rOptVampireOptions o
                   , rOptVampireWorkDir = resolvedVampireWorkDir
                   , rOptProblemFilenameGlobs = rOptProblemFilenameGlobs o
                   , rOptProblemContentFilters = rOptProblemContentFilters o
                   , rOptNumWorkers = rOptNumWorkers o
                   , rOptForce = rOptForce o
                   }


{-
{-
(Old idea)
Two subcommands:

vampire-benchmark run [-n TIMES] OUTPUT-DIR -- vampire -stat on -tstat on file.p
Runs command at the end TIMES times and puts stdout into OUTPUT-DIR/<n>.out
Also note the command used in OUTPUT-DIR/options
(Warn if OUTPUT-DIR exists and especially if options are different! Require -f to add more files in those cases.)
Should also analyse right after (for all fields). Maybe add an option --no-analyse to skip that.

vampire-benchmark analyse [-f FIELD-PATTERN] OUTPUT-DIR
Read vampire output from files OUTPUT-DIR/<n>.out, parse times and print mean, deviation, etc. (like criterion does)
Allow filtering with FIELD-PATTERN (regex?)
-}

data RunOptions = RunOptions
  { rOptDataDir :: FilePath
  , rOptTimes :: Int
  , rOptInvocation :: [String]
  , rOptNoAnalyse :: Bool  -- ^ TODO: run 'analyse' after 'run' with pattern '.*'
  , rOptForce :: Bool
  }
  deriving Show

data AnalyseOptions = AnalyseOptions
  { aOptDataDir :: FilePath
  , aOptPatterns :: [String]
  }
  deriving Show
-}


runOptionsParser :: Int -> Parser UnresolvedRunOptions
runOptionsParser numProcessors =
  pure RunOptions
  <*> dataDirArg
  <*> problemRoot
  <*> vampireExe
  <*> vampireTimeout
  <*> vampireOptions
  <*> vampireWorkDir
  <*> many problemGlob
  <*> many problemRegex
  <*> numWorkers
  <*> forceFlag

  where

    vampireExe =
      strOption $
      short 'x'
      <> long "vampire-exe"
      <> value "vampire"
      <> showDefault
      <> help "Path to vampire executable"
      <> metavar "PATH"

    vampireTimeout =
      option (secondsToDiffTime <$> auto) $
      short 't'
      <> long "vampire-timeout"
      <> value (secondsToDiffTime 60)
      <> showDefault
      <> help "Timeout for each vampire invocation in seconds"
      <> metavar "SECONDS"

    -- TODO: allow quoting or escaping spaces? (doesn't really matter for our usecase)
    vampireOptions =
      option (words <$> str) $
      short 'o'
      <> long "vampire-options"
      <> value [""]
      <> help "Additional options that should be passed to vampire"
      <> metavar "OPTIONS"

    vampireWorkDir =
      strOption $
      long "vampire-workdir"
      <> value "."
      <> showDefault
      <> help "Working directory ($PWD) for vampire subprocesses"
      <> metavar "PATH"

    problemRoot =
      strOption $
      short 'p'
      <> long "problem-root"
      <> help "Specify a directory to run vampire on all problems in that directory subtree."
      -- <> help ("Specify either a directory to run vampire on all problems in that directory subtree, "
      --          <> "or a file to run vampire only on one problem.")
      <> metavar "PATH"

    problemGlob =
      strOption $
      short 'g'
      <> long "problem-glob"
      <> help ("If globs are specified, process only problems whose filenames match at least one of "
               <> "the given glob patterns.")
      <> metavar "GLOB"

    problemRegex =
      strOption $
      short 'r'
      <> long "problem-regex"
      <> help "A problem is only processed if its file content matches all of the given regex filters."
      <> metavar "REGEX"

    numWorkers =
      option auto $
      short 'n'
      <> long "num-workers"
      <> value numProcessors
      <> showDefault
      <> help "The number of vampire instances to run in parallel"

    forceFlag =
      switch $
      long "force"
      <> help "Continue despite warnings"

    dataDirArg =
      strArgument $
      help "Path to directory where vampire output should be stored"
      <> metavar "DATA-DIR"


-- | Not yet implemented (TODO)
analyseOptionsParser :: Parser AnalyseOptions
analyseOptionsParser = empty


commandParser :: Int -> Parser UnresolvedCommand
commandParser numProcessors =
  subparser (command "run" cmdRunParserInfo
             <> command "analyse" cmdAnalyseParserInfo)

  where

    cmdRunParserInfo =
      info
      (Run <$> runOptionsParser numProcessors <**> helper)
      (progDesc "Run vampire benchmarks")

    cmdAnalyseParserInfo =
      info
      (Analyse <$> analyseOptionsParser <**> helper)
      (progDesc "Analyse vampire benchmark results")


optionsParser :: Int -> Parser UnresolvedOptions
optionsParser numProcessors =
  pure Options
  <*> commandParser numProcessors
  <*> debugFlag
  <*> verboseFlag

  where

    debugFlag =
      switch $
      long "debug"
      <> help "Print some debug output"

    verboseFlag =
      switch $
      long "verbose"
      <> short 'v'
      <> help "Print more detailed output"


optionsParserInfo :: Int -> ParserInfo UnresolvedOptions
optionsParserInfo numProcessors =
  info (optionsParser numProcessors <**> helper)
       (fullDesc
        <> progDesc "Check correctness of proofs with vampire"
        <> header "vampire-proof-check")


parseOptions :: IO Options
parseOptions = do
  -- NOTE: we want processors and not capabilities, since vampire runs as a separate process
  numProcessors <- max 1 <$> getNumProcessors

  unresolvedOptions <-
    execParser (optionsParserInfo numProcessors)

  resolveOptions unresolvedOptions
