
{-# LANGUAGE FlexibleInstances #-}

module Smten.ExpH.SmtenEH (
    SmtenEH(..),
    ) where

import Control.Monad
import Data.Functor((<$>))
import Data.Maybe

import Smten.Name
import Smten.Bit
import Smten.Sig
import Smten.Type
import Smten.ExpH.ExpH
import Smten.ExpH.Sugar

class (SmtenT a) => SmtenEH a where
    -- | Convert a haskell object to its smten ExpH representation
    smtenEH :: a -> Thunk

    -- | Convert a smten representation to its haskell object
    de_smtenEH :: Thunk -> Maybe a

instance SmtenEH () where
    smtenEH () = unitEH
    de_smtenEH x = de_kconEH (name "()") x >> return ()

instance SmtenEH Bool where
    smtenEH = boolEH
    de_smtenEH = de_boolEH

instance SmtenEH Integer where
    smtenEH = integerEH
    de_smtenEH = de_integerEH

instance SmtenEH Char where
    smtenEH = charEH
    de_smtenEH = de_charEH

-- The SmtenEH for Thunk just passes through the Thunk unchanged.
-- Note, this is different from packing a haskell Thunk into a smten Thunk. It
-- packs a haskell Thunk into whatever smten object that haskell Thunk
-- represents.
instance SmtenT Thunk where
    smtenT _ = error "smtenT on Thunk"

instance SmtenEH Thunk where
    smtenEH = id
    de_smtenEH = return

-- SmtenEH for IO
-- Note: de_smtenEH can return an IO a which will lead to an error if the
-- returned 'a' is not concrete.
instance (SmtenEH a) => SmtenEH (IO a) where
    smtenEH x = ioEH (smtenEH <$> x)
    de_smtenEH e = do
        io <- de_ioEH e
        return $ fromMaybe (error "de_smtenEH IO") . de_smtenEH <$> io

-- SmtenEH for Bit
instance SmtenT Bit where
    smtenT _ = error "smtenT on Bit"

instance SmtenEH Bit where
    smtenEH = bitEH
    de_smtenEH = de_bitEH

