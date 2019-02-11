{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module VampireProofCheck.Types
  ( Expr(..)
  , Declaration(..)
  , Formula(..)
  , Id(..)
  , Statement(..)
  , isAxiom
  , isInference
  , stmtConclusion
  , Proof(..)
  ) where

-- containers
import Data.Map.Strict (Map)

-- text
import Data.Text (Text)


-- | SMTLib expressions (Lisp-like S-Expressions)
data Expr a
  = Value !a
  | SExpr ![Expr a]
  deriving (Eq, Show, Functor)

-- | Type declarations of constants, functions, ...
newtype Declaration = Decl { unDecl :: Expr Text }
  deriving (Show)

-- | A first-order formula
newtype Formula = Formula { unFormula :: Expr Text }
  deriving (Show)

-- | Id of a statement
newtype Id = Id { unId :: Integer }
  deriving (Eq, Ord)
  deriving newtype (Show)

-- | A statement of the proof (axiom or inferred from previous statements).
-- Note that @Inference f []@ states that @f@ is a tautology (and that
-- vampire should check this), which is different from @Axiom f@ (which
-- simply asserts @f@ without checking it).
data Statement
  = Axiom !Formula
  | Inference !Formula ![Id]
  deriving (Show)

isAxiom :: Statement -> Bool
isAxiom (Axiom _) = True
isAxiom _ = False

isInference :: Statement -> Bool
isInference (Inference _ _) = True
isInference _ = False

stmtConclusion :: Statement -> Formula
stmtConclusion (Axiom f) = f
stmtConclusion (Inference f _) = f

data Proof = Proof
  { proofDeclarations :: ![Declaration]
  , proofStatements :: !(Map Id Statement)
  }
  deriving (Show)
