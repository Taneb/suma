module Suma.Types where

import Data.IntMap.Strict (IntMap)
import Data.Vector (Vector)

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
type Formula = Vector Clause
-- A mapping from variable to the truth value it has been assigned
type Assignment = IntMap Bool -- ~ Map Var Bool ~ Var -> Maybe Bool
