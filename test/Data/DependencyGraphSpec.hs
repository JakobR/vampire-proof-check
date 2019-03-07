{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.DependencyGraphSpec
  ( spec
  ) where

-- containers
import qualified Data.Map.Strict as Map

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
  deriving (Eq, Show, Functor, Foldable, Traversable)

$(deriveEq1 ''ThingF)
$(deriveShow1 ''ThingF)


resolveList :: [(Int, ThingF Int)] -> Either (Error Int) [(Int, Fix ThingF)]
resolveList = fmap Map.toList . resolve . Map.fromList


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
