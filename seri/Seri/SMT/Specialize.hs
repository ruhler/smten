
{-# LANGUAGE PatternGuards #-}

module Seri.SMT.Specialize (
    Logic(..), core, specialize,
    ) where

import Debug.Trace

import Data.List (genericLength)
import Data.Maybe (isJust)

import Seri.Name
import Seri.Type
import Seri.Sig
import Seri.Ppr
import Seri.ExpH

data Logic = Logic {
    th_integer :: Bool,
    th_bit :: Bool
}

core :: Logic
core = Logic {
    th_integer = False,
    th_bit = False
  }

-- Specialize an expression for a given logic.
specialize :: Logic -> ExpH -> ExpH
specialize l e =
 let -- specialize an expression assuming all the children are fully
     -- specialized 
     spec :: ExpH -> ExpH
     spec e 
       | Just (Sig _ t, _, v, b) <- de_letEH e
       , shouldinline l t = b v

        -- (let s = v in b) arg
        -- Turns into: (let s = v in b arg)
       | Just (f, arg) <- de_appEH e
       , Just (s, t, v, b) <- de_letEH f =
          let Just (_, ot) = de_arrowT t
          in spec $ letEH s ot v (\x -> spec $ appEH (b x) arg)
       | CaseEH a k y n <- e
       , not (oktype l (typeof a)) =
          case a of
            --   case (let s = v in b) of
            --        k -> y
            --        _ -> n
            -- Goes to:
            --   let s = v
            --   in case b of
            --        k -> y
            --        _ -> n
            _ | Just (s, _, v, b) <- de_letEH a ->
                spec $ letEH s (typeof e) v (\x -> spec $ caseEH (b x) k y n)
              | otherwise -> error $ "TODO: specialize case arg: " ++ pretty a
       | PrimEH _ _ f (a@(CaseEH {}) : xs) <- e
       , not (oktype l (typeof a)) = 
          let f' = lamEH (Sig (name "_x") (typeof a)) (typeof e) $ \a' ->
                     spec $ f (a':xs)
          in spec $ pushfun f' a

       | PrimEH _ _ f (x:a@(CaseEH {}):xs) <- e
       , not (oktype l (typeof a)) =
          let f' = lamEH (Sig (name "_x") (typeof a)) (typeof e) $ \a' ->
                      spec $ f (x:a':xs)
          in spec $ pushfun f' a
       | otherwise = e

     -- Perform function pushing:
     -- Preserves specialization of arguments.
     --
     --    f (case x of { k -> y; _ -> n})
     -- Turns into:
     --    let _f = f in case x of { k -> _f y; _ -> _f n}
     pushfun :: ExpH -> ExpH -> ExpH
     pushfun f (CaseEH x k y n) =
      let yify :: Integer -> ExpH -> ExpH -> ExpH
          yify 0 f x = spec $ appEH f x
          yify n f (LamEH s t b) =
            let ts = de_arrowsT t
                its = take (fromInteger n) ts
                Just (_, fot) = de_arrowT (typeof f)
                ot = arrowsT (its ++ [fot])
            in lamEH s ot $ \x -> spec $ yify (n-1) f (b x)
          yify n f x = error $ "yify got: " ++ pretty x

          kargs = genericLength (de_arrowsT (typeof k)) - 1
          Just (_, ot) = de_arrowT $ typeof f
      in letEH (Sig (name "_f") (typeof f)) ot f $ \fv ->
             caseEH x k (yify kargs fv y) (spec $ appEH fv n)

     me = specialize l
 in spec $ case e of
             LitEH {} -> e
             ConEH n t xs -> ConEH n t (map me xs)
             VarEH {} -> e
             PrimEH _ _ f xs -> f (map me xs)
             AppEH a b i -> AppEH (me a) (me b) (me i)
             LamEH s t f -> lamEH s t $ \x -> me (f x)
             CaseEH x k y n -> caseEH (me x) k (me y) (me n)
             ErrorEH {} -> e

shouldinline :: Logic -> Type -> Bool
shouldinline l t
 | not (oktype l t) = True
 | otherwise = False

-- Return TRUE if the type is supported in the given logic.
oktype :: Logic -> Type -> Bool
oktype l t = or [
    t == boolT,
    th_integer l && t == integerT,
    th_bit l && isJust (de_bitT t)
    ]

