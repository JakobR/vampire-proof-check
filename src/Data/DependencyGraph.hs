{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.DependencyGraph
  ( Error(..)
  , resolve
  , resolveFix
  ) where

-- base
import Control.Monad.ST (runST, ST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- mtl
import Control.Monad.Except (lift, ExceptT, runExceptT, withExceptT, throwError)

-- recursion-schemes
import Data.Functor.Foldable (Base, Corecursive, refix, Fix(..))


-- | Resolve a directed acyclic dependency graph.
resolve
  :: forall f k t. (Traversable f, Ord k, Corecursive t, Base t ~ f)
  => Map k (f k)
  -> Either (Error k) (Map k t)
resolve = fmap (fmap refix) . resolveFix
{-# INLINABLE resolve #-}

resolveFix
  :: forall f k. (Traversable f, Ord k)
  => Map k (f k)
     -- NOTE: an advantage of taking a Map as input is that we don't
     -- have to worry about duplicate keys in the input.
  -> Either (Error k) (Map k (Fix f))
resolveFix input = runST $ do
  refs :: Map k (STRef s (ResolveState f k)) <-
    sequence $ (newSTRef . Todo) <$> input

  let
    resolveOne :: Set k -> k -> STRef s (ResolveState f k) -> ExceptT (InternalError k) (ST s) (Fix f)
    resolveOne visited j ref = do
      val <- lift $ readSTRef ref
      case val of
        Ok x' -> return x'
        Err e -> throwError e
        Todo x -> do
          let lookupDep' = lookupDep (Set.insert j visited) j
          x' <- Fix <$> withExceptT (errorAt j) (traverse lookupDep' x)
          lift $ writeSTRef ref (Ok x')
          return x'

    lookupDep :: Set k -> k -> k -> ExceptT (InternalError k) (ST s) (Fix f)
    lookupDep visited j i
      | i `Set.member` visited =
          throwError (ICycleIncomplete i [])
      | otherwise =
          case refs Map.!? i of
            Nothing -> throwError (IMissing i j)
            Just ref -> resolveOne visited i ref

  runExceptT . withExceptT fromInternal $ Map.traverseWithKey (resolveOne Set.empty) refs
{-# INLINABLE resolveFix #-}

data ResolveState f k
  = Ok !(Fix f)
  | Err !(InternalError k)
  | Todo !(f k)

data Error k
  = Missing !k !k
    -- ^ @'Missing' i j@ means that the item with id @j@ depends on id @i@,
    -- but no item with id @i@ exists.
  | Cycle ![k]
    -- ^ @'Cycle' js@ means the items with ids @js@ form a dependency cycle.
  deriving (Eq, Show)

errorAt :: Eq k => k -> InternalError k -> InternalError k
errorAt j (ICycleIncomplete i js)
  | i == j    = ICycle (j:js)
  | otherwise = ICycleIncomplete i (j:js)
errorAt _ e = e

data InternalError k
  = IMissing !k !k
  | ICycleIncomplete !k ![k]
  | ICycle ![k]
  deriving (Eq, Show)

fromInternal :: InternalError k -> Error k
fromInternal (IMissing i j) = Missing i j
fromInternal (ICycle js) = Cycle js
fromInternal (ICycleIncomplete i js) = error "Data.DependencyGraph.ICycleIncomplete: should not appear" $
                                       Cycle (i:js)
