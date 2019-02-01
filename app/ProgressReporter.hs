{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module ProgressReporter
  ( Config(..)
  , Handle
  , reportStart
  , reportEnd
  , TaskName
  , withNew
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



data Config = Config
  { enabled :: !Bool
    -- ^ If enabled is False, a no-op ProgressReporter will be used.
  , statusLinePrefix :: !String
    -- ^ The prefix is printed on the status line before the list of tasks in progress.
  }


data Handle t = Handle
  { reportStart :: TaskName -> IO t
  , reportEnd :: t -> String -> IO ()
  }


type TaskName = String


data InternalEnv = InternalEnv
  { sendCommand :: !(Command -> STM ())
  , getNextToken :: !(STM Token)
  }


data OutputThreadEnv = OutputThreadEnv
  { statusLinePrefix :: !String
  , readCommand :: !(IO Command)
  }


data Command
  = TaskStart !Token !TaskName
  | TaskEnd !Token !String
  | Stop !String


type Token = Int


create :: Config -> IO (InternalEnv, OutputThreadEnv)
create Config{..} = do
  cmdChan <- newTChanIO
  let sendCommand = writeTChan cmdChan
      readCommand = atomically $ readTChan cmdChan
  nextTokenVar <- newTVarIO 1
  let getNextToken = readTVar nextTokenVar <* modifyTVar' nextTokenVar (+1)
  return (InternalEnv{..}, OutputThreadEnv{..})


reportStart' :: InternalEnv -> TaskName -> IO Token
reportStart' InternalEnv{..} name = atomically $ do
  token <- getNextToken
  sendCommand (TaskStart token name)
  return token


reportEnd' :: InternalEnv -> Token -> String -> IO ()
reportEnd' InternalEnv{..} token msg = atomically $
  sendCommand (TaskEnd token msg)


withNew :: Config -> (forall t. Handle t -> IO a) -> IO a
withNew cfg action
  | enabled cfg = withNewEnabled cfg action
  | otherwise   = action noOpHandle


withNewEnabled :: Config -> (forall t. Handle t -> IO a) -> IO a
withNewEnabled cfg action = do
  -- NOTE: no need to use bracket for `create`, since we don't allocate
  -- any lasting resources (we don't even have corresponding `destroy`)
  (iEnv, oEnv) <- create cfg

  withAsync (outputThread oEnv) $ \outputAsync -> do

    let handle =
          Handle{ reportStart = reportStart' iEnv
                , reportEnd = reportEnd' iEnv
                }

    x <-
      action handle
      `onException` (sendCommandIO iEnv (Stop "Aborted.") >> wait outputAsync)

    sendCommandIO iEnv (Stop "Complete.")
    wait outputAsync

    return x


outputThread :: OutputThreadEnv -> IO ()
outputThread OutputThreadEnv{..} = go mempty
  where
    go :: IntMap TaskName -> IO ()
    go tasks = do
      cmd <- readCommand
      handle cmd tasks

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


updateStatusLine :: String -> IntMap TaskName -> Maybe String -> IO ()
updateStatusLine prefix tasksMap outputMay = do
  clearStatusLine

  case outputMay of
    Just output -> putStrLn output
    Nothing -> pure ()

  let tasks = map snd $ IntMap.toAscList tasksMap
  putStr ("| " <> prefix <> intercalate ", " tasks <> "...")


noOpHandle :: Handle ()
noOpHandle = Handle
  { reportStart = \_ -> pure ()
  , reportEnd = \_ _ -> pure ()
  }


sendCommandIO :: InternalEnv -> Command -> IO ()
sendCommandIO iEnv = atomically . sendCommand iEnv
