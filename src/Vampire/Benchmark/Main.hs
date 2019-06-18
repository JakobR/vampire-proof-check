{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Vampire.Benchmark.Main
  ( main
  , mainWithOptions
  ) where

-- base
import Control.Exception (assert)
import Control.Monad (when)
import System.Exit (ExitCode(..), exitFailure)
import System.IO
  (hClose, IOMode(WriteMode), withFile, hPutStrLn, hPrint, hSetBuffering, stderr, stdout,
   BufferMode(NoBuffering))

-- async
import Control.Concurrent.Async (wait, link, async)

-- mtl
import Control.Monad.Reader

-- path
import Path

-- path-io
import Path.IO (ensureDir, WalkAction(..), doesFileExist, walkDirRel, walkDir)

-- process
import System.Process

-- time
import Data.Time.Clock (diffTimeToPicoseconds, picosecondsToDiffTime, DiffTime)

-- unagi-chan
import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi.Bounded

-- vampire-tools
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
runBenchmarks options runOptions@RunOptions{rOptProblemRoot,rOptNumWorkers} = do
  let queueBound = rOptNumWorkers + 1

  -- Create task channel.
  -- It is a bounded queue so we don't exhaust memory if we are working through a huge directory tree.
  (tasksIn, tasksOut) <-
    Unagi.Bounded.newChan @Task queueBound

  -- Launch workers
  workers <-
    replicateM rOptNumWorkers (async $ worker options runOptions tasksOut)

  -- Link exceptions in workers to current thread
  -- (this means if a worker throws an exception, it will be re-raised in the current thread)
  mapM_ link workers

  -- Walk through problems directory and create tasks
  let createTask :: Path Rel Dir -> [Path Rel Dir] -> [Path Rel File] -> IO (WalkAction Rel)
      createTask dir _subdirs files = do
        forM_ files $ \file -> do
          putStrLn $ "Creating task from file: " <> show file
          p <- mkProblem runOptions (dir </> file)
          isDone <- checkProblemIsDone p
          unless isDone $ Unagi.Bounded.writeChan tasksIn (Process p)
        return (WalkExclude [])
  walkDirRel createTask rOptProblemRoot

  -- Stop workers
  replicateM_ rOptNumWorkers (Unagi.Bounded.writeChan tasksIn Stop)
  mapM_ wait workers



worker :: Options -> RunOptions -> Unagi.Bounded.OutChan Task -> IO ()
worker options runOptions tasksOut = work
  where
    work = do
      task <- Unagi.Bounded.readChan tasksOut
      case task of
        Stop ->
          return ()
        Process problem -> do
          processProblem options runOptions problem
          work


processProblem :: Options -> RunOptions -> Problem -> IO ()
processProblem options RunOptions{rOptVampireExe,rOptVampireOptions,rOptVampireWorkDir,rOptVampireTimeout} problem@Problem{problemFile,problemOutputDir} = do
  ensureDir problemOutputDir

  withFile (fromAbsFile $ problemStdoutFile problem) WriteMode $ \hOut ->
    withFile (fromAbsFile $ problemStderrFile problem) WriteMode $ \hErr -> do

      putStrLn $ "Begin processing problem: " <> show problemFile

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

      (Just hIn, Nothing, Nothing, process) <- createProcess processInfo
      hClose hIn

      exitcode <- waitForProcess process
      writeFile (fromAbsFile $ problemExitcodeFile problem) (show (exitcodeToInt exitcode) <> "\n")

      putStrLn $ "Done processing problem: " <> show problemFile


exitcodeToInt :: ExitCode -> Int
exitcodeToInt ExitSuccess = 0
exitcodeToInt (ExitFailure n) = assert (n /= 0) n

diffTimeToSeconds :: DiffTime -> Integer
diffTimeToSeconds = (`div` 1_000_000_000_000) . diffTimeToPicoseconds

data Task
  = Process !Problem
  | Stop


data Problem = Problem
  { problemFile :: !(Path Abs File)
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
  outputDir <- parseRelDir (toFilePath $ filename file)
  return Problem{ problemFile = rOptProblemRoot </> file
                , problemOutputDir = rOptDataDir </> outputDir
                }

checkProblemIsDone :: Problem -> IO Bool
checkProblemIsDone = doesFileExist . problemExitcodeFile
