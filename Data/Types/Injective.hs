{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

-- |Injective types. This module contains the 'Injective' typeclass and instances
--  for the following equivalence classes:
--
--  * @{Strict Text, Lazy Text, String}@. 'ByteString's are not part of this,
--    since there exists more than one way to turn unicode text into a ByteString
--    (see "Data.Text.Encoding" and "Data.Text.Lazy.Encoding").
--  * @{Whole, Integer}@. Be advices, though, that Peano numbers may contain
--    unobservable infinities (i.e. @infinity = S infinity@) and thus,
--    the conversion to Integer may not terminate.
--  * @{Nat, Natural}@. For finite values, they're extensionally equivalent,
--    but 'Nat' has lazy infinity.
--
--  Additional injections:
--
--  * Maybe to Either (the reverse is not true, since different 'Left' values
--    would all be converted to 'Nothing').
--  * Integers to Rational.
--  * Natural numbers to Integer and Rational.
module Data.Types.Injective where

import qualified Numeric.Natural as N
import Data.Default
import qualified Data.Maybe as M
import qualified Data.Ratio as R
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Numeric.Peano as PN

-- |The class relation between types @a@ and @b@ s.t. @a@ can be injected
--  into @b@.
--
--  The following laws must be fulfilled:
--
--  [@Injectivity@]
--
-- @
-- x \/= y  ==>  (to x) \/= (to y)
-- @
--
--  [@Totality@]
--  @to@ should be a total function. No cheating by it undefined for parts of the set!
class Injective a b where
   -- |Converts a value of type @a@ "losslessly" to one of type @b@.
   to :: forall b1 a1. (b1 ~ b, a1 ~ a) => a -> b

instance Injective a a where
   to = id

-- equivalence class of string types.
instance Injective TS.Text String where to = TS.unpack
instance Injective String TS.Text where to = TS.pack

instance Injective TS.Text TL.Text where to = TL.fromStrict
instance Injective TL.Text TS.Text where to = TL.toStrict

instance Injective TL.Text String where to = TL.unpack
instance Injective String TL.Text where to = TL.pack

-- equivalence class of integer and whole
instance Injective PN.Whole Integer where to = PN.fromPeano
instance Injective Integer PN.Whole where to = fromInteger

-- equivalence class of nat and natural
instance Injective N.Natural PN.Nat where to = fromIntegral
instance Injective PN.Nat N.Natural where to = fromIntegral

-- Maybe to Either
instance Default a => Injective (Maybe b) (Either a b) where
   to = M.maybe (Left def) Right

-- N to Z,R
instance Injective N.Natural Integer where to = toInteger
instance Injective N.Natural PN.Whole where to = flip PN.Whole PN.Pos . fromIntegral
instance Injective N.Natural R.Rational where to = to . toInteger

instance Injective PN.Nat Integer where to = PN.fromPeano
instance Injective PN.Nat PN.Whole where to = flip PN.Whole PN.Pos
instance Injective PN.Nat R.Rational where to = flip (R.%) 1 . fromIntegral

-- Z to R
instance Injective Integer R.Rational where to = flip (R.%) 1
instance Injective PN.Whole R.Rational where
   to (PN.Whole n PN.Pos) = (fromIntegral n) R.% 1
   to (PN.Whole n PN.Neg) = negate (fromIntegral n) R.% 1
