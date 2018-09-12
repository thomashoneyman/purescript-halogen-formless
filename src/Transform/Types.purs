module Formless.Transform.Types where

import Prim.Row as Row
import Record.Builder (Builder)

-- | Represents empty input
data U e i o = U

-- | Represents building some output record from an empty record
type FromScratch r = Builder {} (Record r)

-- | A constraint synonym for Row.Cons and Row.Lacks
class (Row.Cons s t r r', Row.Lacks s r) <= Row1Cons s t r r' | s t r -> r', s r' -> t r
instance row1Cons :: (Row.Cons s t r r', Row.Lacks s r) => Row1Cons s t r r'

