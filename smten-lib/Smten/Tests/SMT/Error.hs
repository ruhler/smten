
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Error (smttests) where

import Smten.Prelude
import Smten.Data.Bit
import Smten.Symbolic
import Smten.Tests.SMT.Test

smttests :: SMTTest ()
smttests = do
   symtesteq "SMT.Error.Bool" (Just ()) $ do
      p <- free_Bool
      assert p
      assert (if p then True else error "Smten.Error.Bool error hit")

   symtesteq "SMT.Error.Integer" (Just ()) $ do
      p <- free_Bool
      assert p
      assert ((if p then 1 else error "Smten.Error.Integer error hit") == (1 :: Integer))

   symtesteq "SMT.Error.Bit" (Just ()) $ do
      p <- free_Bool
      assert p
      assert ((if p then 1 else error "Smten.Error.Bit error hit") == (1 :: Bit 3))

