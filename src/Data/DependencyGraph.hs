{-# LANGUAGE ScopedTypeVariables #-}

module Data.DependencyGraph
  ( Error(..)
  , resolve
  ) where

-- containers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- mtl
import Control.Monad.Except (ExceptT, runExceptT, withExceptT, throwError)
import Control.Monad.Reader (Reader, runReader, asks, local)

-- recursion-schemes
import Data.Functor.Foldable (Fix(..))


-- | Resolve a directed acyclic dependency graph.
resolve
  :: forall f k. (Traversable f, Ord k)
  => Map k (f k)
  -> Either (Error k) (Map k (Fix f))
resolve input =
  let
    resolved :: Map k (ExceptT (InternalError k) (Reader (Set k)) (Fix f))
    resolved = Map.mapWithKey resolveOne input

    resolveOne :: k -> f k -> ExceptT (InternalError k) (Reader (Set k)) (Fix f)
    resolveOne j = fmap Fix
                   . withExceptT (errorAt j)
                   . local (Set.insert j)
                   . traverse (lookupDep j)

    lookupDep :: k -> k -> ExceptT (InternalError k) (Reader (Set k)) (Fix f)
    lookupDep j i =
      case resolved Map.!? i of
        Nothing ->
          throwError (IMissing i j)
        Just getDep -> do
          isLooping <- asks (i `Set.member`)
          if isLooping
            then throwError (ICycleIncomplete i [])
            else getDep

    resolved' :: ExceptT (Error k) (Reader (Set k)) (Map k (Fix f))
    resolved' = withExceptT fromInternal (sequence resolved)
  in
    runReader (runExceptT resolved') Set.empty
{-# INLINABLE resolve #-}

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
-- errorAt j (IMissingIncomplete i) = IMissing i j
errorAt _ e = e

data InternalError k
  -- = IMissingIncomplete !k
  = IMissing !k !k
  | ICycleIncomplete !k ![k]
  | ICycle ![k]
  deriving (Eq, Show)

fromInternal :: InternalError k -> Error k
fromInternal (IMissing i j) = Missing i j
fromInternal (ICycle js) = Cycle js
-- fromInternal (IMissingIncomplete _) =  error "Data.DependencyGraph.IMissingIncomplete: should not appear"
fromInternal (ICycleIncomplete i js) = error "Data.DependencyGraph.ICycleIncomplete: should not appear" $
                                       Cycle (i:js)
