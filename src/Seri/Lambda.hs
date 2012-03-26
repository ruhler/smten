
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Seri.Lambda (
    Name, VarE(..), LamE(..), AppE(..)
    ) where

import Seri.Elaborate

data VarE = VarE Name
    deriving(Show, Eq)

instance (Inject VarE e) => Elaborate VarE e where
    elaborate x = inject x
    reduce n v (VarE nm) | n == nm = v
    reduce _ _ v = inject v
    

data LamE e = LamE Name e
    deriving(Show, Eq)

instance (Inject (LamE e) e, Elaborate e e)
        => Elaborate (LamE e) e where
    elaborate x = inject x
    reduce n v (LamE nm b) | n /= nm = reduce n v b
    reduce _ _ l = inject l
    
data AppE e = AppE e e
    deriving(Show, Eq)

instance (Inject (AppE e) e, Inject (LamE e) e, Elaborate e e)
        => Elaborate (AppE e) e where
    elaborate (AppE a b) = 
        let a' = elaborate a :: e
            b' = elaborate b :: e
        in case (unject a') of
              Just (LamE name body :: LamE e) -> elaborate $ reduce name b body
              Nothing -> inject $ (AppE a' b' :: AppE e)

    reduce n v (AppE a b) =
        let a' = reduce n v a
            b' = reduce n v b
        in inject $ AppE a' b'
        
    
