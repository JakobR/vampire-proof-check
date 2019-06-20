{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Vampire.Benchmark.Main
  ( main
  , mainWithOptions
  ) where

-- base
import Control.Concurrent (threadDelay)
import Control.Exception (onException, assert)
import Control.Monad (when)
import System.Exit (ExitCode(..), exitFailure)
import System.IO
  (Handle, hClose, IOMode(WriteMode), withFile, hPutStrLn, hPrint, hSetBuffering, stderr, stdout,
   BufferMode(NoBuffering))

-- async
import Control.Concurrent.Async (race, wait, link, async)

-- managed
import Control.Monad.Managed

-- mtl
import Control.Monad.Reader

-- path
import Path

-- path-io
import Path.IO (ensureDir, WalkAction(..), doesFileExist, walkDirRel, walkDir)

-- process
import System.Process

-- time
import Data.Time.Clock (secondsToDiffTime, diffTimeToPicoseconds, picosecondsToDiffTime, DiffTime)
import Data.Time.Format (formatTime)
import Data.Time.LocalTime (getZonedTime)

-- unagi-chan
import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi.Bounded

-- vampire-tools
import qualified System.Console.ProgressReporter as ProgressReporter
import Vampire.Benchmark.Options



data Logger = Logger

data Env co = Env
  { envOptions :: Options
  , envCmdOptions :: co
  , logger :: Logger
  }


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  opts <- parseOptions
  mainWithOptions opts


mainWithOptions :: Options -> IO ()
mainWithOptions opts@Options{optDebug,optCommand} = do
  when optDebug $ hPrint stderr opts

  case optCommand of
    Run runOpts ->
      runBenchmarks opts runOpts
    Analyse _ -> do
      hPutStrLn stderr "analyse: not yet implemented"
      exitFailure


runBenchmarks :: Options -> RunOptions -> IO ()
runBenchmarks options@Options{optVerbose} runOptions@RunOptions{rOptProblemRoot,rOptNumWorkers} = do

  let prCfg = ProgressReporter.Config
        { enabled = optVerbose
        , statusLinePrefix = "Benchmarking: "
        , outputHandle = stdout
        }

  ProgressReporter.withNew prCfg $ \pr -> do
    let queueBound = rOptNumWorkers + 1

    -- Create task channel.
    -- It is a bounded queue so we don't exhaust memory if we are working through a huge directory tree.
    (tasksIn, tasksOut) <-
      Unagi.Bounded.newChan @Task queueBound

    -- Launch workers
    workers <-
      replicateM rOptNumWorkers (async $ worker options runOptions pr tasksOut)

    -- Link exceptions in workers to current thread
    -- (this means if a worker throws an exception, it will be re-raised in the current thread)
    mapM_ link workers

    -- Walk through problems directory and create tasks
    let createTask :: Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> IO (WalkAction Rel)
        createTask dir _subdirs files = do
          forM_ files $ \file -> do
            -- putStrLn $ "Creating task from file: " <> show (dir </> file)
            p <- mkProblem runOptions (dir </> file)
            isDone <- checkProblemIsDone p
            unless isDone $ Unagi.Bounded.writeChan tasksIn (Process p)
          return (WalkExclude [])
    walkDirRel createTask rOptProblemRoot

    -- Stop workers
    replicateM_ rOptNumWorkers (Unagi.Bounded.writeChan tasksIn Stop)
    mapM_ wait workers



worker :: Options -> RunOptions -> ProgressReporter.Handle t -> Unagi.Bounded.OutChan Task -> IO ()
worker options runOptions pr tasksOut = work
  where
    work = do
      task <- Unagi.Bounded.readChan tasksOut
      case task of
        Stop ->
          return ()
        Process problem -> do
          processProblem options runOptions pr problem
          work


processProblem :: Options -> RunOptions -> ProgressReporter.Handle t -> Problem -> IO ()
processProblem options RunOptions{rOptVampireExe,rOptVampireOptions,rOptVampireWorkDir,rOptVampireTimeout} pr problem@Problem{problemFile,problemName,problemOutputDir} = runManaged $ do

  ensureDir problemOutputDir

  token <- managed $ ProgressReporter.withTask pr problemName (problemName <> ": error")

  hOut <- managed $ withFile (fromAbsFile $ problemStdoutFile problem) WriteMode
  hErr <- managed $ withFile (fromAbsFile $ problemStderrFile problem) WriteMode

  let
    vampireArgs =
      [ "--time_limit", show (diffTimeToSeconds rOptVampireTimeout) ]
      ++ rOptVampireOptions
      ++ [ fromAbsFile problemFile ]

    defaultProcessInfo =
      proc rOptVampireExe vampireArgs

    processInfo =
      defaultProcessInfo
      { cwd = Just (fromAbsDir rOptVampireWorkDir)
      , env = Nothing
      , std_in = CreatePipe
      , std_out = UseHandle hOut
      , std_err = UseHandle hErr
      , close_fds = True
      }

  handles <- managed $ withCreateProcess' processInfo
  case handles of
    (Just hIn, Nothing, Nothing, process) -> liftIO $ do
      hClose hIn

      -- Wait up to ten seconds until the vampire process is killed when it goes overtime
      let tolerance = secondsToDiffTime 10

      eExitcode <-
        race (waitForProcess process
              `onException` terminateProcess process)
             (threadDelay (diffTimeToMicroseconds $ rOptVampireTimeout + tolerance)
              >> terminateProcess process)  -- TODO: send SIGKILL in this case?

      endTime <- formatTime (error "no time locale") "%0Y-%m-%d %H:%M:%S %Z" <$> getZonedTime
      let msgPrefix = "[" <> endTime <> "] " <> problemName <> ": "

      case eExitcode of
        Left exitcode -> do
          writeFile (fromAbsFile $ problemExitcodeFile problem) (show (exitcodeToInt exitcode) <> "\n")
          -- TODO: Extract result to szs file
          ProgressReporter.reportEnd pr token (msgPrefix <> "done")  -- TODO: more info
        Right () -> do
          ProgressReporter.reportEnd pr token (msgPrefix <> "killed vampire because it got stuck")

    _ ->
      error "impossible"


exitcodeToInt :: ExitCode -> Int
exitcodeToInt ExitSuccess = 0
exitcodeToInt (ExitFailure n) = assert (n /= 0) n

diffTimeToSeconds :: DiffTime -> Integer
diffTimeToSeconds = (`div` 1_000_000_000_000) . diffTimeToPicoseconds

diffTimeToMicroseconds :: DiffTime -> Int
diffTimeToMicroseconds = fromIntegral . (`div` 1_000_000) . diffTimeToPicoseconds

-- killProcess :: ProcessHandle -> IO ()
-- killProcess process = do
--   mpid <- getPid process
--   case mpid of
--     Nothing -> return ()
--     Just pid -> error "TODO"

withCreateProcess' :: CreateProcess -> ((Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO r) -> IO r
withCreateProcess' cp f = withCreateProcess cp (curry4 f)
{-# INLINE withCreateProcess' #-}

curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f a b c d = f (a, b, c, d)
{-# INLINE curry4 #-}

data Task
  = Process !Problem
  | Stop


data Problem = Problem
  { problemName :: ! String
  , problemFile :: !(Path Abs File)
  , problemOutputDir :: !(Path Abs Dir)
  }
  deriving Show

problemExitcodeFile :: Problem -> Path Abs File
problemExitcodeFile Problem{problemOutputDir} =
  problemOutputDir </> [relfile|exitcode|]

problemStdoutFile :: Problem -> Path Abs File
problemStdoutFile Problem{problemOutputDir} =
  problemOutputDir </> [relfile|out|]

problemStderrFile :: Problem -> Path Abs File
problemStderrFile Problem{problemOutputDir} =
  problemOutputDir </> [relfile|err|]

problemSZSFile :: Problem -> Path Abs File
problemSZSFile Problem{problemOutputDir} =
  problemOutputDir </> [relfile|szs|]

mkProblem :: RunOptions -> Path Rel File -> IO Problem
mkProblem RunOptions{rOptProblemRoot,rOptDataDir} file = do
  outputDir <- parseRelDir (fromRelFile file)
  return Problem{ problemName = toFilePath file
                , problemFile = rOptProblemRoot </> file
                , problemOutputDir = rOptDataDir </> outputDir
                }

checkProblemIsDone :: Problem -> IO Bool
checkProblemIsDone = doesFileExist . problemExitcodeFile
