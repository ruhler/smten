
{-# LANGUAGE PatternGuards #-}

module Smten.ExpH.SmtenEHs2 () where

import Data.Maybe

import Smten.Type
import Smten.Lit
import Smten.Sig
import Smten.Name
import Smten.Ppr
import Smten.ExpH.ExpH
import Smten.ExpH.Ppr
import Smten.ExpH.SmtenEH
import Smten.ExpH.Sugar

-- SmtenEH for (a -> b)
-- Notes:
-- * smtenEH uses smtenT on the argument type a.
--   So you can't pack (ExpH -> b), for example.
-- * smtenEH packs into something which doesn't support non-concrete evaluation.
-- * de_smtenEH can return a (a -> b) which will lead to an error if the
--   returned 'b' is not concrete.
instance (SmtenEH a, SmtenEH b) => SmtenEH (a -> b) where
    smtenEH f =
      lamEH (smtenT f) (name "x") $ \x ->
         smtenEH $ f (fromMaybe (error "smtenEH (->)") (de_smtenEH x))
            
    -- TODO: Verify this is a function type?
    de_smtenEH e =
      let Just (_, to) = de_arrowT (smtenT e)
      in return $ \x ->
           let fx = appEH to e (smtenEH x)
           in fromMaybe (error $ "de_smtenEH (->): " ++ pretty fx) (de_smtenEH fx)

