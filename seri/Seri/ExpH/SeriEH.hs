
{-# LANGUAGE FlexibleInstances #-}

module Seri.ExpH.SeriEH (
    SeriEH(..),
    ) where

import Control.Monad
import Data.Functor((<$>))
import Data.Maybe

import Seri.Name
import Seri.Bit
import Seri.Sig
import Seri.Type
import Seri.ExpH.ExpH
import Seri.ExpH.Sugar

class (SeriT a) => SeriEH a where
    -- | Convert a haskell object to its seri ExpH representation
    seriEH :: a -> ExpH

    -- | Convert a seri representation to its haskell object
    de_seriEH :: ExpH -> Maybe a

instance SeriEH () where
    seriEH () = unitEH
    de_seriEH x = de_kconEH (name "()") x >> return ()

instance SeriEH Bool where
    seriEH = boolEH
    de_seriEH = de_boolEH

instance SeriEH Integer where
    seriEH = integerEH
    de_seriEH = de_integerEH

instance SeriEH Char where
    seriEH = charEH
    de_seriEH = de_charEH

-- The SeriEH for ExpH just passes through the ExpH unchanged.
-- Note, this is different from packing a haskell ExpH into a seri ExpH. It
-- packs a haskell ExpH into whatever seri object that haskell ExpH
-- represents.
instance SeriT ExpH where
    seriT _ = error "seriT on ExpH"

instance SeriEH ExpH where
    seriEH = id
    de_seriEH = return

-- SeriEH for IO
-- Note: de_seriEH can return an IO a which will lead to an error if the
-- returned 'a' is not concrete.
instance (SeriEH a) => SeriEH (IO a) where
    seriEH x = ioEH (seriEH <$> x)
    de_seriEH e = do
        io <- de_ioEH e
        return $ fromMaybe (error "de_seriEH IO") . de_seriEH <$> io

-- SeriEH for Bit
instance SeriT Bit where
    seriT _ = error "seriT on Bit"

instance SeriEH Bit where
    seriEH = bitEH
    de_seriEH = de_bitEH

