{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplictForall #-}

-- |Contains the class definition of 'Iso', indicating isomorphism between two
--  types.
module Data.Types.Isomorphic (
   Iso(..),
   module Data.Types.Injective,
   from,
   ) where

import qualified Numeric.Natural as N
import qualified Numeric.Peano as PN
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

import Data.Types.Injective

-- |The class of isomorphic types, i.e. those which can be cast to each
--  other without loss of information. Type isomorphism is an equivalence
--  relation (reflexive, symmetric, transitive), but due to the limitations
--  of the type system, only reflexivity is implemented for all types.
--  Since there are no type inequality constraints, writing symmetry and
--  transitivity instances over all types would result in overlapping instances
--  with due to reflexivity.
--
--  The following must be ensured:
--
--  [@Isomorphism@]
-- @
-- from . to = id
-- @
--
--  Reflexivity, symmetry and transitivity are then "free":
--
-- @
-- instance Iso a a 
-- @
--
-- @
-- instance (Iso a b, Iso b c) => Iso a c 
-- @
--
--  Out of these, only the first one (reflexivity) is actually implemented,
--  since the other two would result in overlapping instances. We would be able
--  to avoid this with type inequality constrains (e.g. @a \/~ b@, @a \/~ c@,
--  @b \/~ c)@.
class (Injective a b, Injective b a) => Iso a b where

-- |Synonym for 'to'.
from :: forall b a. (Iso a b) => b -> a
from = to

instance Iso a a where

-- equivalence class of string types.
instance Iso TS.Text String
instance Iso String TS.Text

instance Iso TL.Text String
instance Iso String TL.Text

instance Iso TS.Text TL.Text
instance Iso TL.Text TS.Text

-- Peano wholes and integers.
instance Iso PN.Whole Integer
instance Iso Integer PN.Whole
