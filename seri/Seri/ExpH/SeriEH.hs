
module Seri.ExpH.SeriEH (
    SeriEH(..),
    ) where

import Control.Monad

import Seri.Name
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

    de_seriEH e = do
        Sig n _ <- de_conEH e
        guard $ n == name "()"
        return ()

instance SeriEH Bool where
    seriEH = boolEH
    de_seriEH = de_boolEH

instance SeriEH Integer where
    seriEH = integerEH
    de_seriEH = de_integerEH

instance SeriEH Char where
    seriEH = charEH
    de_seriEH = de_charEH

