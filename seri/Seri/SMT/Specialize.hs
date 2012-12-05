
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
 let me = specialize l

     -- specialize just the children of the given expression.
     sub :: ExpH -> ExpH
     sub e 
      | LitEH {} <- e = e
      | ConEH n t xs <- e = ConEH n t (map me xs)
      | VarEH {} <- e = e
      | PrimEH s f xs <- e = f (map me xs)
      | AppEH a b <- e = appEH (me a) (me b)
      | LamEH s f <- e = lamEH s $ \x -> me (f x)
      | CaseEH x k y n <- e = caseEH (me x) k (me y) (me n)
      | ErrorEH {} <- e = e
          
 in case sub e of
     e | Just (_, v, b) <- de_letEH e
       , shouldinline l v -> me (b v)
       | Just (f, arg) <- de_appEH e
       , Just (s, v, b) <- de_letEH f
           -> me $ letEH s v (\x -> appEH (b x) arg)
       | Just (f@(CaseEH {}), arg) <- de_appEH e
       , not (oktype l (typeof f)) -> me $ pusharg f arg
       | CaseEH a@(CaseEH {}) k y n <- e
       , not (oktype l (typeof a)) -> me $
          let f = lamEH (Sig (name "_x") (typeof a)) $ \a' -> caseEH a' k y n
          in pushfun f a
       | PrimEH _ f (a@(CaseEH {}) : xs) <- e
       , not (oktype l (typeof a)) -> me $
          let f' = lamEH (Sig (name "_x") (typeof a)) $ \a' -> f (a':xs)
          in pushfun f' a
       | PrimEH _ f (x:a@(CaseEH {}):xs) <- e
       , not (oktype l (typeof a)) -> me $
          let f' = lamEH (Sig (name "_x") (typeof a)) $ \a' -> f (x:a':xs)
          in pushfun f' a
       | otherwise -> e

shouldinline :: Logic -> ExpH -> Bool
shouldinline l v
 | LitEH {} <- v = True
 | ConEH {} <- v = True
 | VarEH {} <- v = True
 | ErrorEH {} <- v = True
 | not (oktype l (typeof v)) = True
 | otherwise = False

-- Perform argument pushing.
-- (case a of
--     k -> y
--     _ -> n) arg
-- Where y = \v1 -> \v2 -> ... -> yv
-- Translates to:
--  let z = arg
--  in case a of
--         k -> \v1 -> \v2 -> ... -> yv z
--         _ -> n z
pusharg :: ExpH -> ExpH -> ExpH
pusharg (CaseEH a k y n) arg =
 let yify :: Integer -> (ExpH -> ExpH) -> ExpH -> ExpH
     yify 0 f x = f x
     yify n f (LamEH s b) = lamEH s $ \x -> yify (n-1) f (b x)
     yify n f x = error $ "yify got: " ++ pretty x

     kargs = genericLength (de_arrowsT (typeof k)) - 1
 in letEH (Sig (name "_z") (typeof arg)) arg $ \av -> 
        let ybody = \yv -> appEH yv av
            y' = yify kargs ybody y
            n' = appEH n av
        in caseEH a k y' n'

-- Return TRUE if the type is supported in the given logic.
oktype :: Logic -> Type -> Bool
oktype l t = or [
    t == boolT,
    th_integer l && t == integerT,
    th_bit l && isJust (de_bitT t)
    ]
 
-- Perform function pushing:
--    f (case x of { k -> y; _ -> n})
-- Turns into:
--    let _f = f in case x of { k -> _f y; _ -> _f n}
pushfun :: ExpH -> ExpH -> ExpH
pushfun f (CaseEH x k y n) =
 let yify :: Integer -> ExpH -> ExpH -> ExpH
     yify 0 f x = appEH f x
     yify n f (LamEH s b) = lamEH s $ \x -> yify (n-1) f (b x)
     yify n f x = error $ "yify got: " ++ pretty x

     kargs = genericLength (de_arrowsT (typeof k)) - 1
 in letEH (Sig (name "_f") (typeof f)) f $ \fv ->
        caseEH x k (yify kargs fv y) (appEH fv n)

