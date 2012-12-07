
{-# LANGUAGE PatternGuards #-}

module Seri.SMT.Simpline (simpline) where

import Seri.ExpH

-- Inline all the simple arguments in a post-specialized expression.
simpline :: ExpH -> ExpH
simpline =
  let f :: ExpH -> Maybe ExpH
      f e | Just (_, _, v, f) <- de_letEH e 
          , issimple v = Just (simpline (f v))
          | otherwise = Nothing
  in transform f

issimple :: ExpH -> Bool
issimple e
 | LitEH {} <- e = True
 | VarEH {} <- e = True
 | ConEH _ _ [] <- e = True
 | ErrorEH {} <- e = True
 | otherwise = False

