{-# LANGUAGE ScopedTypeVariables #-}

module VampireProofCheck.List
  ( findDuplicate
  , hasDuplicates
  ) where

-- base
import Data.List (sort)


hasDuplicates :: forall a . Ord a => [a] -> Bool
hasDuplicates = go . sort
  where go :: [a] -> Bool
        go (x:xs@(y:_)) = x == y || go xs
        go _ = False

findDuplicate :: forall a . Ord a => [a] -> Maybe a
findDuplicate = go . sort
  where go :: [a] -> Maybe a
        go (x:xs@(y:_))
          | x == y    = Just x
          | otherwise = go xs
        go _ = Nothing

