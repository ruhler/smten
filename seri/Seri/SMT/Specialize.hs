
{-# LANGUAGE PatternGuards #-}

module Seri.SMT.Specialize (
    Logic(..), core,
    specialize,
    ) where

import Data.List (genericLength)

import Seri.Name
import Seri.Type
import Seri.Sig
import Seri.Ppr
import Seri.ExpH

data Logic = Logic {
    th_integer :: Bool,
    th_bit :: Bool,
    th_lambda :: Bool
}

core :: Logic
core = Logic {
    th_integer = False,
    th_bit = False,
    th_lambda = False
  }
    
-- Specialize an expression for a given logic.
-- TODO: actually perform specialization.
specialize :: Logic -> ExpH -> ExpH
specialize l e =
 let me = specialize l
 in case () of
     _ | not (th_lambda l)
       , Just (f@(CaseEH a k y n), arg) <- de_appEH e -> me $ pusharg f arg
       | otherwise -> e

-- Perform argument pushing.
-- (case a of
--     k -> y
--     _ -> n) arg
-- Where y = \v1 -> \v2 -> ... -> yv
-- Translates to:
--     case a of
--         k -> \v1 -> \v2 -> ... -> yv arg
--         _ -> n arg
pusharg :: ExpH -> ExpH -> ExpH
pusharg (CaseEH a k y n) arg =
 let yify :: Integer -> (ExpH -> ExpH) -> ExpH -> ExpH
     yify 0 f x = f x
     yify n f (LamEH s b) = lamEH s $ \x -> yify (n-1) f (b x)
     yify n f x = error $ "yify got: " ++ pretty x

     kargs = genericLength (de_arrowsT (typeof k)) - 1

     lam = lamEH (Sig (name "_z") (typeof arg)) $ \av ->
        let ybody = \yv -> appEH yv av
            y' = yify kargs ybody y
            n' = appEH n av
        in caseEH a k y' n'
  in appEH lam arg

