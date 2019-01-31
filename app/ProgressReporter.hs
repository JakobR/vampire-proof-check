{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module ProgressReporter
  ( ProgressReporter(..)
  , withProgressReporter
  , withProgressReporterIf
  , TaskName
  ) where

-- base
import Control.Exception (onException)
import Data.List (intercalate)

-- ansi-terminal
import System.Console.ANSI (setCursorColumn, clearLine)

-- async
import Control.Concurrent.Async (withAsync, wait)

-- containers
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

-- stm
import Control.Concurrent.STM


data ProgressReporter t = ProgressReporter
  { reportStart :: TaskName -> IO t
  , reportEnd :: t -> String -> IO ()
  }


data Message
  = TaskStart !Token !TaskName
  | TaskEnd !Token !String
  | Stop !String


data Internal = Internal
  { sendMessage :: !(Message -> STM ())
  , getNextToken :: !(STM Token)
  }


data OutputEnv = OutputEnv
  { statusLinePrefix :: !String
  , readMessage :: !(IO Message)
  }


type Token = Int
type TaskName = String
type Tasks = IntMap TaskName


create :: String -> IO (Internal, OutputEnv)
create statusLinePrefix = do
  messageChan <- newTChanIO
  let sendMessage = writeTChan messageChan
      readMessage = atomically $ readTChan messageChan
  nextTokenVar <- newTVarIO 1
  let getNextToken = readTVar nextTokenVar <* modifyTVar' nextTokenVar (+1)
  return (Internal{..}, OutputEnv{..})


reportStart' :: Internal -> TaskName -> IO Token
reportStart' Internal{..} name = atomically $ do
  token <- getNextToken
  sendMessage (TaskStart token name)
  return token


reportEnd' :: Internal -> Token -> String -> IO ()
reportEnd' Internal{..} token msg = atomically $
  sendMessage (TaskEnd token msg)


sendMessageIO :: Internal -> Message -> IO ()
sendMessageIO i = atomically . sendMessage i


outputThread :: OutputEnv -> IO ()
outputThread OutputEnv{..} = go mempty
  where
    go :: Tasks -> IO ()
    go tasks = do
      msg <- readMessage
      handle msg tasks

    handle (TaskStart token name) oldTasks = do
      let newTasks = IntMap.insert token name oldTasks
      updateStatusLine statusLinePrefix newTasks Nothing
      go newTasks

    handle (TaskEnd token msg) oldTasks = do
      let newTasks = IntMap.delete token oldTasks
      updateStatusLine statusLinePrefix newTasks (Just msg)
      go newTasks

    -- handle (PrintLine msg) tasks = do
    --   updateStatusLine statusLinePrefix tasks (Just msg)
    --   go tasks

    handle (Stop msg) _ = do
      clearStatusLine
      putStrLn msg
      -- NOTE: No call to `go` since we want to stop here.


clearStatusLine :: IO ()
clearStatusLine = do
  setCursorColumn 0
  clearLine


putStatusLine :: String -> IntMap TaskName -> IO ()
putStatusLine prefix tasksMap = do
  let tasks = map snd $ IntMap.toAscList tasksMap
  putStr ("| " <> prefix <> intercalate ", " tasks <> "...")


updateStatusLine :: String -> IntMap TaskName -> Maybe String -> IO ()
updateStatusLine prefix tasksMap outputMay = do
  clearStatusLine
  case outputMay of
    Just output -> putStrLn output
    Nothing -> pure ()
  putStatusLine prefix tasksMap


withProgressReporter :: String -> (forall t. ProgressReporter t -> IO a) -> IO a
withProgressReporter statusLinePrefix action = do

  (i, oEnv) <- create statusLinePrefix

  withAsync (outputThread oEnv) $ \outputAsync -> do

    let progressReporter =
          ProgressReporter{ reportStart = reportStart' i
                          , reportEnd = reportEnd' i
                          }

    x <-
      action progressReporter
      `onException` (sendMessageIO i (Stop "Aborted.") >> wait outputAsync)

    sendMessageIO i (Stop "Complete.")
    wait outputAsync

    return x


withProgressReporterIf :: Bool -> String -> (forall t. ProgressReporter t -> IO a) -> IO a
withProgressReporterIf True statusLinePrefix action = withProgressReporter statusLinePrefix action
withProgressReporterIf False _ action = action dummyProgressReporter


dummyProgressReporter :: ProgressReporter ()
dummyProgressReporter = ProgressReporter
  { reportStart = \_ -> pure ()
  , reportEnd = \_ _ -> pure ()
  }
