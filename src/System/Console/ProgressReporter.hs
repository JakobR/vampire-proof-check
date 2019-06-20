{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module System.Console.ProgressReporter
  ( Config(..)
  , Handle
  , reportStart
  , reportEnd
  , putMessage
  , withTask
  , TaskName
  , withNew
  ) where

-- base
import Control.Exception (bracket, onException)
import Control.Monad (when)
import Data.List (intercalate)
import System.IO (hPutStrLn, hPutStr)
import qualified System.IO

-- ansi-terminal
import System.Console.ANSI (hClearLine, hSetCursorColumn, hSupportsANSI)

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
  , outputHandle :: !System.IO.Handle
    -- ^ The progress messages will be printed to this handle (usually stdout or stderr).
  }


data Handle t = Handle
  { reportStart :: TaskName -> IO t
  , reportEnd :: t -> String -> IO ()
  , putMessage :: String -> IO ()
  }


withTask
  :: Handle t
  -> TaskName
  -> String  -- ^ default task end message to display
  -> (t -> IO r)
  -> IO r
withTask h name defaultEndMessage = do
  bracket
    (reportStart h name)
    (\t -> reportEnd h t defaultEndMessage)


type TaskName = String


data InternalEnv = InternalEnv
  { sendCommand :: !(Command -> STM ())
  , getNextToken :: !(STM Token)
  }


data OutputThreadEnv = OutputThreadEnv
  { readCommand :: !(IO Command)
  , statusLinePrefix :: !String
  , outputHandle :: !System.IO.Handle
  , useStatusLine :: !Bool
  }


data Command
  = TaskStart !Token !TaskName
  | TaskEnd !Token !String
  | Message !String
  | Stop !String


type Token = Int


create :: Config -> IO (InternalEnv, OutputThreadEnv)
create Config{..} = do
  cmdChan <- newTChanIO
  let sendCommand = writeTChan cmdChan
      readCommand = atomically $ readTChan cmdChan
  nextTokenVar <- newTVarIO 1
  let getNextToken = readTVar nextTokenVar <* modifyTVar' nextTokenVar (+1)
  useStatusLine <- hSupportsANSI outputHandle
  return (InternalEnv{..}, OutputThreadEnv{..})


reportStart' :: InternalEnv -> TaskName -> IO Token
reportStart' InternalEnv{getNextToken,sendCommand} name = atomically $ do
  token <- getNextToken
  sendCommand (TaskStart token name)
  return token


reportEnd' :: InternalEnv -> Token -> String -> IO ()
reportEnd' InternalEnv{sendCommand} token msg = atomically $
  sendCommand (TaskEnd token msg)


putMessage' :: InternalEnv -> String -> IO ()
putMessage' InternalEnv{sendCommand} msg = atomically $
  sendCommand (Message msg)


withNew :: Config -> (forall t. Handle t -> IO a) -> IO a
withNew cfg action
  | enabled cfg = withNewEnabled cfg action
  | otherwise   = action noOpHandle


withNewEnabled :: Config -> (forall t. Handle t -> IO a) -> IO a
withNewEnabled cfg action = do
  -- NOTE: no need to use bracket for `create`, since we don't allocate
  -- any lasting resources (we don't even have a corresponding `destroy`)
  (iEnv, oEnv) <- create cfg

  withAsync (outputThread oEnv) $ \outputAsync -> do

    let handle =
          Handle{ reportStart = reportStart' iEnv
                , reportEnd = reportEnd' iEnv
                , putMessage = putMessage' iEnv
                }

    x <-
      action handle
      `onException` (sendCommandIO iEnv (Stop "Aborted.")
                     >> wait outputAsync)

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
      when useStatusLine $
        updateStatusLine newTasks Nothing
      go newTasks

    handle (TaskEnd token msg) oldTasks
      | token `IntMap.member` oldTasks = do
          let newTasks = IntMap.delete token oldTasks
          if useStatusLine
            then updateStatusLine newTasks (Just msg)
            else hPutStrLn outputHandle msg
          go newTasks
      | otherwise =
          -- TaskEnd for this task has already been reported
          go oldTasks

    handle (Message msg) tasks = do
      if useStatusLine
        then updateStatusLine tasks (Just msg)
        else hPutStrLn outputHandle msg
      go tasks

    handle (Stop msg) _ = do
      when useStatusLine clearStatusLine
      hPutStrLn outputHandle msg
      -- NOTE: No call to `go` since we want to stop here.

    clearStatusLine :: IO ()
    clearStatusLine = do
      hSetCursorColumn outputHandle 0
      hClearLine outputHandle

    updateStatusLine :: IntMap TaskName -> Maybe String -> IO ()
    updateStatusLine tasksMap outputMay = do
      clearStatusLine
      case outputMay of
        Just output -> hPutStrLn outputHandle output
        Nothing -> pure ()
      let tasks = map snd $ IntMap.toAscList tasksMap
          statusLine = "| " <> statusLinePrefix <> intercalate ", " tasks <> "..."
      hPutStr outputHandle statusLine


noOpHandle :: Handle ()
noOpHandle = Handle
  { reportStart = \_ -> pure ()
  , reportEnd = \_ _ -> pure ()
  , putMessage = \_ -> pure ()
  }


sendCommandIO :: InternalEnv -> Command -> IO ()
sendCommandIO iEnv = atomically . sendCommand iEnv
