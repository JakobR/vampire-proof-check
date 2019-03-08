{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.DependencyGraphSpec
  ( spec
  ) where

-- base
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Data.Either (isRight)
import GHC.Generics (Generic1, Generic)

-- async
import Control.Concurrent.Async (race)

-- containers
import qualified Data.Map.Strict as Map

-- deepseq
import Control.DeepSeq (NFData, NFData1, force)

-- deriving-compat
import Data.Eq.Deriving (deriveEq1)
import Text.Show.Deriving (deriveShow1)

-- hspec
import Test.Hspec

-- recursion-schemes
import Data.Functor.Foldable (Fix(..))

-- vampire-proof-check
import Data.DependencyGraph


data ThingF a = Thing String [a]
  deriving (Eq, Show, Functor, Foldable, Generic, Generic1, Traversable)
  deriving anyclass NFData
  deriving anyclass NFData1

-- -- Orphan instances for tests
-- deriving stock instance Generic (Error k)
-- deriving anyclass instance NFData k => NFData (Error k)
-- deriving stock instance Generic (Fix f)
-- deriving anyclass instance (forall x. NFData x => NFData (f x)) => NFData (Fix f)

$(deriveEq1 ''ThingF)
$(deriveShow1 ''ThingF)


resolveList :: [(Int, ThingF Int)] -> Either (Error Int) [(Int, Fix ThingF)]
resolveList = fmap Map.toList . resolve . Map.fromList


-- TODO: should use something like Benchmarkable in package criterion to ensure the computation
-- isn't optimized away.
-- Maybe we should just use criterion to run performance tests like this.
shouldCompleteWithin :: HasCallStack => IO a -> Int -> Expectation
shouldCompleteWithin action timeout = do
  result <- race action (threadDelay timeout)
  case result of
    Left _ -> return ()
    Right () -> expectationFailure ("did not complete within " <> show timeout <> "µs")


shouldEvaluateWithin :: (HasCallStack, NFData a) => a -> Int -> Expectation
shouldEvaluateWithin x timeout = do
  evaluate (force x) `shouldCompleteWithin` timeout


-- | The thing with id 1 has Θ(bf^n) dependencies.
exponentialDependencies :: Int -> Int -> [(Int, ThingF Int)]
exponentialDependencies bf n =
  [ (i, Thing ("t" <> show i) (if i < n then replicate bf (i+1) else [])) | i <- [1 .. n] ]


spec :: Spec
spec =
  describe "resolve" $ do
    it "works on correct input" $
      resolveList [ (1, Thing "a" [4])
                  , (2, Thing "b" [1,3])
                  , (3, Thing "c" [])
                  , (4, Thing "d" [3])
                  ]
        `shouldBe`
        let thing1 = Fix (Thing "a" [thing4])
            thing2 = Fix (Thing "b" [thing1, thing3])
            thing3 = Fix (Thing "c" [])
            thing4 = Fix (Thing "d" [thing3])
        in Right [(1, thing1), (2, thing2), (3, thing3), (4, thing4)]

    it "properly handles missing dependencies" $
      resolveList [ (1, Thing "a" [2])
                  , (2, Thing "b" [3])
                  ]
        `shouldBe`
        Left (Missing 3 2)

    it "properly handles missing dependencies (2)" $
      resolveList [ (3, Thing "a" [2])
                  , (2, Thing "b" [1])
                  ]
        `shouldBe`
        Left (Missing 1 2)

    it "discovers dependency cycles" $
      resolveList [ (1, Thing "a" [1])
                  ]
        `shouldBe`
        Left (Cycle [1])

    it "discovers dependency cycles (2)" $
      resolveList [ (1, Thing "a" [2])
                  , (2, Thing "b" [3,5])
                  , (3, Thing "c" [5,4])
                  , (4, Thing "d" [2])
                  , (5, Thing "e" [])
                  ]
        `shouldBe`
        Left (Cycle [2,3,4])

    it "runs in linear time" $ do
      -- We don't 'deepseq' the actual result, since it seems to do complete traversal of the data structure,
      -- which will take Θ(2^1000) for this example (but memory usage of the fully evaluated value would
      -- still be linear due to sharing).
      -- Hence we just call isRight on the result. This does effectively the same work, since all
      -- dependencies have to be resolved before deciding whether to return an error or not.
      -- Since the value is valid, we don't return early due to an error.
      isRight (resolveList (exponentialDependencies 2 1000))
        `shouldEvaluateWithin` 100_000
      isRight (resolveList (exponentialDependencies 2 1000))
        `shouldBe` True
