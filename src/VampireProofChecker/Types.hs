{-# LANGUAGE DeriveFunctor #-}

module VampireProofChecker.Types
  ( Expr(..)
  , Declaration(..)
  , Formula(..)
  , Id(..)
  , Statement(..)
  , stmtConclusion
  , Proof(..)
  ) where

-- containers
import Data.Map ( Map )

-- text
import Data.Text ( Text )


-- | SMTLib expressions (Lisp-like S-Expressions)
data Expr a
  = Value a
  | SExpr [Expr a]
  deriving (Eq, Show, Functor)

-- | Type declarations of constants, functions, ...
newtype Declaration = Decl { unDecl :: Expr Text }
  deriving (Show)

-- | A first-order formula
newtype Formula = Formula { unFormula :: Expr Text }
  deriving (Show)

-- | Id of a statement
newtype Id = Id { unId :: Integer }
  deriving (Eq, Ord, Show)

-- | A statement of the proof (axiom or inferred from previous statements)
data Statement
  = Axiom Formula
  | Inference Formula [Id]
  deriving (Show)

stmtConclusion :: Statement -> Formula
stmtConclusion (Axiom f) = f
stmtConclusion (Inference f _) = f

data Proof = Proof
  { proofDeclarations :: [Declaration]
  , proofStatements :: Map Id Statement
  }
  deriving (Show)
