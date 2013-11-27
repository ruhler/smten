
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Error (smttests) where

import Smten.Prelude
import Smten.Data.Bit
import Smten.Symbolic
import Smten.Tests.SMT.Test

smttests :: SMTTest ()
smttests = do
   -- Smten.Error.NoFair
   --  Unreachable errors should certainly not be reached,
   --  regardless of whether Smten guarentees fairness or no
   symtesteq "SMT.Error.NoFair" (Just ()) $ do
      p <- free_Bool
      assert p
      assert (if p then True else error "SMT.Error.NoFair error hit")

   -- Smten.Error.Fair
   --  Smten guarentees fairness, meaning an error should not be reached if
   --  there is a satisfying assignment.
   symtesteq "SMT.Error.Fair1" (Just ()) $ do
      p <- free_Bool
      assert (if p then True else error "SMT.Error.Fair1 error hit")

   symtesteq "SMT.Error.Fair2" (Just ()) $ do
      p <- free_Bool
      assert (if p then error "SMT.Error.Fair2 error hit" else True)

   symtesteq "SMT.Error.Integer" (Just ()) $ do
      p <- free_Bool
      assert p
      assert ((if p then 1 else error "SMT.Error.Integer error hit") == (1 :: Integer))

   symtesteq "SMT.Error.Bit" (Just ()) $ do
      p <- free_Bool
      assert p
      assert ((if p then 1 else error "SMT.Error.Bit error hit") == (1 :: Bit 3))

