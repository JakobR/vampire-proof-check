{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module VampireProofCheck.Types
  ( Expr(..)
  , Declaration(..)
  , Formula(..)
  , Id(..)
  , StatementF(..)
  , Statement(..)
  , isAxiomF
  , isAxiom
  , isInferenceF
  , isInference
  , stmtFormulaF
  , stmtFormula
  , stmtPremisesF
  , stmtPremises
  , Proof(..)
  ) where

-- base
import GHC.Generics (Generic)

-- containers
import Data.Map.Strict (Map)

-- deriving-compat
import Text.Show.Deriving (deriveShow1)

-- recursion-schemes
import Data.Functor.Foldable (Base, Recursive, Corecursive, project)

-- text
import Data.Text (Text)


-- | SMTLib expressions (Lisp-like S-Expressions)
data Expr a
  = Value !a
  | SExpr ![Expr a]
  deriving stock (Eq, Show, Functor)

-- | Type declarations of constants, functions, ...
newtype Declaration = Decl { unDecl :: Expr Text }
  deriving stock Show

-- | A first-order formula
newtype Formula = Formula { unFormula :: Expr Text }
  deriving stock Show

-- | Id of a statement
newtype Id = Id { unId :: Integer }
  deriving stock (Eq, Ord)
  deriving newtype Show

-- | A statement of the proof (axiom or inferred from previous statements).
-- Note that @Inference f []@ states that @f@ is a tautology (and that
-- vampire should check this), which is different from @Axiom f@ (which
-- simply asserts @f@ without checking it).
data StatementF a
  = AxiomF !Formula
  | InferenceF !Formula ![a]
  deriving stock (Show, Generic, Functor, Foldable, Traversable)

$(deriveShow1 ''StatementF)

-- If the list were strict in its elements, circular dependencies would not be possible.
-- TODO: investigate how we can use that.
data Statement
  = Axiom !Formula
  | Inference !Formula ![Statement]
  deriving stock (Show, Generic)
  deriving anyclass (Recursive, Corecursive)

type instance Base Statement = StatementF

isAxiomF :: StatementF a -> Bool
isAxiomF (AxiomF _) = True
isAxiomF _ = False

isAxiom :: Statement -> Bool
isAxiom = isAxiomF . project

isInferenceF :: StatementF a -> Bool
isInferenceF (InferenceF _ _) = True
isInferenceF _ = False

isInference :: Statement -> Bool
isInference = isInferenceF . project

stmtFormulaF :: StatementF a -> Formula
stmtFormulaF (AxiomF f) = f
stmtFormulaF (InferenceF f _) = f

-- | The formula derived by the given statement
stmtFormula :: Statement -> Formula
stmtFormula = stmtFormulaF . project

stmtPremisesF :: StatementF a -> [a]
stmtPremisesF (AxiomF _) = []
stmtPremisesF (InferenceF _ fs) = fs

stmtPremises :: Statement -> [Statement]
stmtPremises = stmtPremisesF . project

data Proof = Proof
  { proofDeclarations :: ![Declaration]
  , proofStatements :: !(Map Id Statement)
  }
  deriving stock Show
