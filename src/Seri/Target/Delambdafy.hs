-------------------------------------------------------------------------------
-- Copyright (c) 2012      SRI International, Inc. 
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
-- ("CTSRD"), as part of the DARPA CRASH research programme.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
-------------------------------------------------------------------------------
--
-- Authors: 
--   Richard Uhler <ruhler@csail.mit.edu>
-- 
-------------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

-- | Target for making sure any lambdas are fully applied if possible in a
-- seri expression.
module Seri.Target.Delambdafy (
    delambdafy
   ) where

import Data.List(nub)

import Seri.Lambda

delambdafy :: Exp -> Exp
delambdafy = delambdafy' []

delambdafy' :: [(Name, Exp)] -> Exp -> Exp
delambdafy' reds e = 
  case e of
    LitE {} -> e
    ConE {} -> e
    VarE (Sig n _) | Just v <- lookup n reds -> v
    VarE {} -> e

    AppE a bs ->
     case (delambdafy' reds a, map (delambdafy' reds) bs) of
        -- Push application inside laces where possible.
        -- Rewrite:    ((\a -> f) x) y
        --             ((\a -> f y) x)
        -- With proper renaming.
        (AppE c@(LaceE {}) x, y) ->
           let intom (Match ps b) = Match ps (AppE b y)
               LaceE ms = alpharename (concatMap free' y) c
           in delambdafy' reds $ AppE (LaceE (map intom ms)) x

        (a, b) -> AppE a b

    LaceE ms ->
      let dom (Match p b) = 
            let sigs = concatMap bindingsP p
                names = [n | Sig n _ <- sigs]
                nreds = [(n, VarE s) | s@(Sig n _) <- sigs]
            in Match p (delambdafy' (nreds++reds) b)
          ms' = map dom ms 
      in LaceE ms'

-- Return True if the given expression binds the given name in a lambda term
-- somewhere.
hasname :: Name -> Exp -> Bool
hasname n =
  let hn :: Exp -> Bool
      hn (LitE {}) = False
      hn (ConE {}) = False
      hn (VarE {}) = False
      hn (AppE a b) = hn a || any hn b
      hn (LaceE ms) =
        let hnp :: Pat -> Bool
            hnp (ConP _ _ ps) = any hnp ps
            hnp (VarP (Sig nm _)) = n == nm
            hnp (LitP {}) = False
            hnp (WildP {}) = False
            
            hnm :: Match -> Bool
            hnm (Match p b) = any hnp p || hn b
        in any hnm ms
  in hn

-- | Rename any variable bindings in the given expression to names which do
-- not belong to the given list.
alpharename :: [Name] -> Exp -> Exp
alpharename [] e = e
alpharename bad e =
  let isgood :: Name -> Bool
      isgood s = not (s `elem` bad)

      isgoodnew :: Name -> Bool
      isgoodnew s = isgood s && not (hasname s e)

      -- get the new name for the given name.
      newname :: Name -> Name
      newname n | isgood n = n
      newname n = head (filter isgoodnew [n `nappend` name (show i) | i <- [0..]])
    
      repat :: Pat -> Pat
      repat (ConP t n ps) = ConP t n (map repat ps)
      repat (VarP (Sig n t)) = VarP (Sig (newname n) t)
      repat p@(LitP {}) = p
      repat p@(WildP {}) = p

      rematch :: [Name] -> Match -> Match
      rematch bound (Match ps b) = 
        let ps' = map repat ps
            b' = rename (concatMap bindingsP' ps ++ bound) b
        in Match ps' b'

      -- Do alpha renaming in an expression given the list of bound variable
      -- names before renaming.
      rename :: [Name] -> Exp -> Exp
      rename _ e@(LitE {}) = e
      rename _ e@(ConE {}) = e
      rename bound (VarE (Sig n t)) | n `elem` bound = VarE (Sig (newname n) t) 
      rename _ e@(VarE {}) = e
      rename bound (AppE a b) = AppE (rename bound a) (map (rename bound) b)
      rename bound (LaceE ms)
        = LaceE (map (rematch bound) ms)
  in rename [] e


isfunt :: Type -> Bool
isfunt t = 
  case unarrowsT t of
      _:_:_ -> True
      _ -> False

deleteall :: Eq a => a -> [a] -> [a]
deleteall k [] = []
deleteall k (x:xs) | k == x = deleteall k xs
deleteall k (x:xs) = x : deleteall k xs

