{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Types.Injective where

import qualified Numeric.Natural as N
import qualified Numeric.Natural.Internal as N (runNatural)
import Data.Default
import qualified Data.Maybe as M
import qualified Data.Ratio as R
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Numeric.Peano as PN

-- |The class relation between types @a@ and @b@ s.t. @a@ can be injected
--  into @b@.
--
--  The following laws must be fulfilled:
--  * 'to' is total.
--  * @x /= y  ==>  (to x) /= (to y)@.
class Injective a b where
   to :: a -> b

instance Injective a a where
   to = id

-- equivalence class of string types.
instance Injective TS.Text String where to = TS.unpack
instance Injective String TS.Text where to = TS.pack

instance Injective TL.Text String where to = TL.unpack
instance Injective String TL.Text where to = TL.pack

instance Injective TS.Text TL.Text where to = TL.fromStrict
instance Injective TL.Text TS.Text where to = TL.toStrict


-- integers and naturals.
instance Injective N.Natural Integer where to = N.runNatural
instance Injective Integer R.Rational where to = flip (R.%) 1
instance Default a => Injective (Maybe b) (Either a b) where
   to = M.maybe (Left def) Right

instance Injective PN.Nat Integer where to = PN.fromPeano
instance Injective PN.Whole Integer where to = PN.fromPeano
instance Injective Integer PN.Whole where to = fromInteger

instance Injective PN.Nat PN.Whole where to = flip PN.Whole PN.Pos
