module Suma.Types where

import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)

-- Throughout we use [] as both a container and a non-determinism monad.

-- A variable, e.g. p, q, r...
type Var = Int
-- A parity, either False (¬) or True (not ¬)
type Parity = Bool
-- A literal, e.g. ¬p, q, ¬r
type Literal = (Int, Bool)
-- A sequence of literals, which are combined by ||
type Clause = [Literal]
-- A sequence of clauses, which are combined by &&
type Formula = [Clause]
-- A mapping from variable to the truth value it has been assigned
type Assignment = IntMap Bool -- ~ Map Var Bool ~ Var -> Maybe Bool
-- A mapping from variable to the set of (indices of) clauses that contain it
type OccurrenceLists = IntMap IntSet
-- A set of clauses with 1 unassigned literal
type Readys = IntMap Literal
