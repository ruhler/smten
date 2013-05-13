
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Class for SMT solver's abstract syntax tree.
module Smten.SMT.AST (AST(..)) where

import qualified Smten.HashTable as HT
import Smten.Name
import Smten.Lit

class AST ctx exp | ctx -> exp where
  assert :: ctx -> exp -> IO ()
  literal :: ctx -> Lit -> IO exp
  bool :: ctx -> Bool -> IO exp
  var :: ctx -> Name -> IO exp
  ite :: ctx -> exp -> exp -> exp -> IO exp
  unary :: HT.HashTable Name (ctx -> exp -> IO exp)
  binary :: HT.HashTable Name (ctx -> exp -> exp -> IO exp)
  zeroextend :: ctx -> exp -> Integer -> IO exp
  signextend :: ctx -> exp -> Integer -> IO exp
  extract :: ctx -> exp -> Integer -> Integer -> IO exp
  truncate :: ctx -> exp -> Integer -> IO exp
  
