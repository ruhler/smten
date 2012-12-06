
module Seri.ExpH.SeriEHs2 () where

import Data.Maybe

import Seri.Type
import Seri.Lit
import Seri.Sig
import Seri.Name
import Seri.Ppr
import Seri.ExpH.ExpH
import Seri.ExpH.Ppr
import Seri.ExpH.SeriEH
import Seri.ExpH.Sugar

-- SeriEH for (a -> b)
-- Notes:
-- * seriEH uses seriT on the argument type a.
--   So you can't pack (ExpH -> b), for example.
-- * seriEH packs into something which doesn't support non-concrete evaluation.
-- * de_seriEH can return a (a -> b) which will lead to an error if the
--   returned 'b' is not concrete.
instance (SeriEH a, SeriEH b) => SeriEH (a -> b) where
    seriEH f =
      let ta :: (a -> b) -> a
          ta _ = undefined
        
          tb :: (a -> b) -> b
          tb _ = undefined
          
      in lamEH (Sig (name "x") (seriT (ta f))) (seriT (tb f)) $ \x ->
            seriEH $ f (fromMaybe (error "seriEH (->)") (de_seriEH x))
            
    de_seriEH (LamEH _ _ f) = return $ \x ->
       let fx = f (seriEH x)
       in fromMaybe (error $ "de_seriEH (->): " ++ pretty fx) (de_seriEH fx)
    de_seriEH _ = Nothing

