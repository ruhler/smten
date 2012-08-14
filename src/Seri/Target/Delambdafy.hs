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

-- | Target for making sure any lambdas are fully applied if possible in a
-- seri expression.
module Seri.Target.Delambdafy (
    delambdafy
   ) where

import Data.List(nub)

import Seri.Lambda

delambdafy :: Exp -> Exp
delambdafy = delambdafy' []

delambdafy' :: [Name] -> Exp -> Exp
delambdafy' nms e = 
  case e of
    LitE {} -> e
    CaseE x ms ->
      let ms' = [Match p (delambdafy' (bindingsP' p ++ nms) b) | Match p b <- ms]
      in CaseE (delambdafy' nms x) ms'

    -- Always perform beta reduction if the argument is a function type.
    AppE (LamE (Sig name _) b) x | isfunt (typeof x) ->
       let renamed = alpharename (deleteall name nms) b
       in delambdafy' nms (reduce name x renamed)

    -- Push application inside lambdas where possible.
    -- Rewrite:    ((\a -> b) x) y
    --             ((\a -> b y) x)
    -- With proper renaming.
    AppE (AppE c@(LamE {}) x) y ->
       let LamE s b = alpharename nms c
       in delambdafy' nms $ AppE (LamE s (AppE b y)) x

    -- Push application inside of case where possible.
    -- Rewrite:   (case x of { p1 -> m1; p2 -> m2 ; ... }) y
    --            (case x of { p1 -> m1 y; p2 -> m2 y; ... })
    -- With proper renaming
    AppE c@(CaseE {}) y -> 
      let CaseE x ms = alpharename nms c
      in delambdafy' nms $ CaseE x [Match p (AppE b y) | Match p b <- ms]
    
    AppE a b -> AppE (delambdafy' nms a) (delambdafy' nms b)
    LamE s@(Sig n t) b -> LamE s (delambdafy' (n:nms) b)
    ConE {} -> e
    VarE {} -> e

-- reduce n v exp
-- Perform beta reduction in exp, replacing occurrences of variable n with v.
reduce :: Name -> Exp -> Exp -> Exp
reduce n v =
  let reduceme e =
        case e of
           LitE {} -> e
           CaseE e ms ->
             let reducematch :: Match -> Match
                 reducematch m@(Match p _) | n `elem` (bindingsP' p) = m
                 reducematch (Match p b) = Match p (reduceme b)
             in CaseE (reduceme e) (map reducematch ms)
           AppE a b -> AppE (reduceme a) (reduceme b)
           LamE (Sig nm t) b | n == nm -> e
           LamE s b -> LamE s (reduceme b)
           ConE {} -> e
           VarE (Sig nm _) | n == nm -> v
           VarE {} -> e
  in reduceme

-- | Return a list of all local variable names introduced in the given
-- expression.
names :: Exp -> [Name]
names (LitE {}) = []
names (CaseE e ms) = 
  let namesm :: Match -> [Name]
      namesm (Match p b) = bindingsP' p ++ names b
  in nub $ concat (names e : map namesm ms)
names (AppE a b) = names a ++ names b
names (LamE (Sig n _) b) = nub $ n : names b
names (ConE {}) = []
names (VarE (Sig n _)) = []


-- | Rename any variable bindings in the given expression to names which do
-- not belong to the given list.
alpharename :: [Name] -> Exp -> Exp
alpharename [] e = e
alpharename bad e =
  let isgood :: String -> Bool
      isgood s = not (s `elem` bad)

      isgoodnew :: String -> Bool
      isgoodnew s = isgood s && not (s `elem` names e)

      -- get the new name for the given name.
      newname :: String -> String
      newname n | isgood n = n
      newname n = head (filter isgoodnew [n ++ show i | i <- [0..]])
    
      repat :: Pat -> Pat
      repat (ConP t n ps) = ConP t n (map repat ps)
      repat (VarP (Sig n t)) = VarP (Sig (newname n) t)
      repat p@(IntegerP {}) = p
      repat p@(WildP {}) = p

      rematch :: [Name] -> Match -> Match
      rematch bound (Match p b) = 
        let p' = repat p
            b' = rename (bindingsP' p ++ bound) b
        in Match p' b'

      -- Do alpha renaming in an expression given the list of bound variable
      -- names before renaming.
      rename :: [Name] -> Exp -> Exp
      rename _ e@(LitE {}) = e
      rename bound (CaseE e ms)
        = CaseE (rename bound e) (map (rematch bound) ms)
      rename bound (AppE a b) = AppE (rename bound a) (rename bound b)
      rename bound (LamE (Sig n t) b)
        = LamE (Sig (newname n) t) (rename (n : bound) b)
      rename _ e@(ConE {}) = e
      rename bound (VarE (Sig n t)) | n `elem` bound = VarE (Sig (newname n) t) 
      rename _ e@(VarE {}) = e
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

