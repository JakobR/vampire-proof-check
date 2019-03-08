{-# LANGUAGE ScopedTypeVariables #-}

module VampireProofCheck.List
  ( findDuplicate
  , findDuplicate'
  , findDuplicateOn
  , hasDuplicates
  ) where

-- base
import Data.List (group, sortBy, sort)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

-- safe
import Safe (headMay)


hasDuplicates :: forall a . Ord a => [a] -> Bool
hasDuplicates = go . sort
  where go :: [a] -> Bool
        go (x:xs@(y:_)) = x == y || go xs
        go _ = False
{-# INLINABLE hasDuplicates #-}


findDuplicate :: forall a . Ord a => [a] -> Maybe a
findDuplicate = go . sort
  where go :: [a] -> Maybe a
        go (x:xs@(y:_))
          | x == y    = Just x
          | otherwise = go xs
        go _ = Nothing
{-# INLINABLE findDuplicate #-}


findDuplicate' :: Ord a => [a] -> Maybe (a, a)
findDuplicate' = headMay . mapMaybe atLeast2 . group . sort
  where
    atLeast2 [] = Nothing
    atLeast2 [_] = Nothing
    atLeast2 (x:y:_) = Just (x, y)
{-# INLINABLE findDuplicate' #-}


findDuplicateOn :: forall a b. Ord b => (a -> b) -> [a] -> Maybe (a, a)
findDuplicateOn f = go . sortBy (comparing fstP) . map (\x -> P (f x) x)
  where go :: [P b a] -> Maybe (a, a)
        go ( (P b1 a1) : xs@((P b2 a2) : _) )
          | b1 == b2    = Just (a1, a2)
          | otherwise = go xs
        go _ = Nothing
{-# INLINABLE findDuplicateOn #-}

data P b a = P !b !a

fstP :: P b a -> b
fstP (P b _) = b
