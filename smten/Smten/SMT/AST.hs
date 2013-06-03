
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Class for SMT solver's abstract syntax tree.
module Smten.SMT.AST (AST(..)) where

class AST ctx exp | ctx -> exp where
  assert :: ctx -> exp -> IO ()
  bool :: ctx -> Bool -> IO exp
  var :: ctx -> String -> IO exp
  ite :: ctx -> exp -> exp -> exp -> IO exp

