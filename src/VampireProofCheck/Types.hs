{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module VampireProofCheck.Types
  ( Expr(..)
  , Declaration(..)
  , Formula(..)
  , Id(..)
  , StatementF(..)
  , ParsedStatement
  , Statement
  , Fix(..)
  , isAxiomF
  , isAxiom
  , isInferenceF
  , isInference
  , stmtConclusionF
  , stmtConclusion
  , stmtPremisesF
  , stmtPremises
  , Proof(..)
  ) where

-- containers
import Data.Map.Strict (Map)

-- deriving-compat
import Text.Show.Deriving (deriveShow1)

-- recursion-schemes
import Data.Functor.Foldable (unfix, Fix(..))

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
data StatementF a
  = Axiom !Formula
  | Inference !Formula ![a]
  deriving (Show, Functor, Foldable, Traversable)

$(deriveShow1 ''StatementF)

-- What the parser returns
type ParsedStatement = (Id, StatementF Id)

-- What the program wants to work with
type Statement = Fix StatementF

isAxiomF :: StatementF a -> Bool
isAxiomF (Axiom _) = True
isAxiomF _ = False

isAxiom :: Statement -> Bool
isAxiom = isAxiomF . unfix

isInferenceF :: StatementF a -> Bool
isInferenceF (Inference _ _) = True
isInferenceF _ = False

isInference :: Statement -> Bool
isInference = isInferenceF . unfix

stmtConclusionF :: StatementF a -> Formula
stmtConclusionF (Axiom f) = f
stmtConclusionF (Inference f _) = f

stmtConclusion :: Statement -> Formula
stmtConclusion = stmtConclusionF . unfix

stmtPremisesF :: StatementF a -> [a]
stmtPremisesF (Axiom _) = []
stmtPremisesF (Inference _ fs) = fs

stmtPremises :: Statement -> [Statement]
stmtPremises = stmtPremisesF . unfix

data Proof = Proof
  { proofDeclarations :: ![Declaration]
  , proofStatements :: !(Map Id Statement)
  }
  deriving (Show)
