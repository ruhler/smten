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

delambdafy' :: [(Name, Exp)] -> Exp -> Exp
delambdafy' reds e = 
  case e of
    LitE {} -> e
    CaseE x ms ->
      let dom (Match p b) = 
            let sigs = bindingsP p
                names = [n | Sig n _ <- sigs]
                nreds = [(n, VarE s) | s@(Sig n _) <- sigs]
            in Match p (delambdafy' (nreds++reds) b)
          ms' = map dom ms 
      in CaseE (delambdafy' reds x) ms'

    AppE a b ->
     case (delambdafy' reds a, delambdafy' reds b) of
        -- Always perform beta reduction if the argument is a function type.
        (l@(LamE {}), x) | isfunt (typeof x) ->
           let LamE (Sig name _) b = alpharename (free' x) l
           in delambdafy' ((name, x):reds) b

        -- Push application inside lambdas where possible.
        -- Rewrite:    ((\a -> b) x) y
        --             ((\a -> b y) x)
        -- With proper renaming.
        (AppE c@(LamE {}) x, y) ->
           let LamE s b = alpharename (free' y) c
           in delambdafy' reds $ AppE (LamE s (AppE b y)) x

        -- Push application inside of case where possible.
        -- Rewrite:   (case x of { p1 -> m1; p2 -> m2 ; ... }) y
        --            (case x of { p1 -> m1 y; p2 -> m2 y; ... })
        -- With proper renaming
        (c@(CaseE {}), y) -> 
          let CaseE x ms = alpharename (free' y) c
          in delambdafy' reds $ CaseE x [Match p (AppE b y) | Match p b <- ms]
    
        (a, b) -> AppE a b
    LamE s@(Sig n t) b -> LamE s (delambdafy' ((n, VarE s):reds) b)
    ConE {} -> e
    VarE (Sig n t) ->
      case lookup n reds of
         Just v -> v
         Nothing -> e

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

