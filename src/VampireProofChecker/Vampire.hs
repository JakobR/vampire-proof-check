{-# LANGUAGE ScopedTypeVariables #-}

module VampireProofChecker.Vampire
  ( runVampire
  , Seconds
  , VampireResult(..)
  ) where

-- base
import Data.List ( intercalate, isPrefixOf )
import System.Exit ( ExitCode(..) )

-- process
import System.Process ( readProcessWithExitCode )

-- text
import Data.Text ( Text )
import qualified Data.Text as Text

-- vampire-proof-checker
import VampireProofChecker.Types


type Seconds = Int


showExpr :: Expr Text -> String
showExpr (Value v) = Text.unpack v
showExpr (SExpr xs) = "(" <> intercalate " " (showExpr <$> xs) <> ")"

runVampire
  :: FilePath     -- ^ path to vampire executable
  -> Seconds      -- ^ vampire timeout (0 means no limit)
  -> [Expr Text]  -- ^ the expressions to pass to vampire
  -> IO VampireResult
runVampire vampireExe timeoutSecs inputExprs =
  runVampire' vampireExe timeoutSecs input
  where
    input = intercalate "\n" (showExpr <$> inputExprs)

runVampire'
  :: FilePath  -- ^ path to vampire executable
  -> Seconds   -- ^ vampire timeout (0 means no limit)
  -> String    -- ^ the input to pass to vampire
  -> IO VampireResult
runVampire' vampireExe timeoutSecs input = do
  let options = [ "--proof", "off"
                , "--mode", "portfolio"
                , "--statistics", "none"
                , "--time_limit", show timeoutSecs]
  (exitCode, output, _err) <- readProcessWithExitCode vampireExe options input
  return $ parseVampireOutput exitCode output

parseVampireOutput :: ExitCode -> String -> VampireResult
parseVampireOutput exitCode output =
  case (exitCode, reasons) of
    (ExitSuccess, ["Theorem"]) -> Refutation
    (ExitSuccess, ["CounterSatisfiable"]) -> Satisfiable
    (_, ["Timeout"]) -> Timeout
    (_, _) -> Error ("ExitCode = " <> show exitCode
                      <> "; Unknown termination reason: " <> show reasons
                      <> "\nOutput:\n" <> output)
  where
    reasonPrefix = "% SZS status "
    extractReason = head . (++["<none>"]) . take 1 . words . drop (length reasonPrefix)
    reasonLines = filter (reasonPrefix `isPrefixOf`) (lines output)
    reasons = extractReason <$> reasonLines

data VampireResult
  = Satisfiable
  | Refutation
  | Timeout
  | Error String
  deriving (Eq, Show)
