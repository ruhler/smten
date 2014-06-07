
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Smten.Tests.SMT.Error (smttests) where

import Smten.Prelude
import Smten.Data.Bit
import Smten.Search
import Smten.Tests.SMT.Test

smttests :: SMTTest ()
smttests = do
   -- Smten.Error.NoFair
   --  Unreachable errors should certainly not be reached,
   --  regardless of whether Smten guarentees fairness or no
   symtesteq "SMT.Error.NoFair" (Just ()) $ do
      p <- free_Bool
      guard p
      guard (if p then True else error "SMT.Error.NoFair error hit")

   -- Smten.Error.Fair
   --  Smten guarentees fairness, meaning an error should not be reached if
   --  there is a satisfying assignment.
   symtesteq "SMT.Error.Fair1" (Just ()) $ do
      p <- free_Bool
      guard (if p then True else error "SMT.Error.Fair1 error hit")

   symtesteq "SMT.Error.Fair2" (Just ()) $ do
      p <- free_Bool
      guard (if p then error "SMT.Error.Fair2 error hit" else True)

   symtesteq "SMT.Error.IntegerNoFair" (Just ()) $ do
      p <- free_Bool
      guard p
      guard ((if p then 1 else error "SMT.Error.IntegerNoFair error hit") == (1 :: Integer))

   symtesteq "SMT.Error.IntegerFair1" (Just ()) $ do
      p <- free_Bool
      guard ((if p then 1 else error "SMT.Error.IntegerFair1 error hit") == (1 :: Integer))

   symtesteq "SMT.Error.IntegerFair2" (Just ()) $ do
      p <- free_Bool
      guard ((if p then error "SMT.Error.IntegerFair1 error hit" else 1) == (1 :: Integer))

   symtesteq "SMT.Error.BitNoFair" (Just ()) $ do
      p <- free_Bool
      guard p
      guard ((if p then 1 else error "SMT.Error.BitNoFair error hit") == (1 :: Bit 3))

   symtesteq "SMT.Error.BitFair1" (Just ()) $ do
      p <- free_Bool
      guard ((if p then 1 else error "SMT.Error.BitFair1 error hit") == (1 :: Bit 3))

   symtesteq "SMT.Error.BitFair2" (Just ()) $ do
      p <- free_Bool
      guard ((if p then error "SMT.Error.BitFair2 error hit" else 1) == (1 :: Bit 3))

   symtesteq "SMT.Error.SymNoFair" (Just ()) $ do
      p <- free_Bool
      guard p
      if p
        then return ()
        else error "SMT.Error.SymNoFair error hit"

   symtesteq "SMT.Error.SymFair1" (Just ()) $ do
      p <- free_Bool
      if p
        then return ()
        else error "SMT.Error.SymFair1 error hit"

   symtesteq "SMT.Error.SymFair2" (Just ()) $ do
      p <- free_Bool
      if p
        then error "SMT.Error.SymFair2 error hit"
        else return ()

   symtesteq "SMT.Error.IntNoFair" (Just ()) $ do
      p <- free_Bool
      guard p
      guard ((if p then 1 else error "SMT.Error.IntNoFair error hit") == (1 :: Int))

   symtesteq "SMT.Error.IntFair1" (Just ()) $ do
      p <- free_Bool
      guard ((if p then 1 else error "SMT.Error.IntFair1 error hit") == (1 :: Int))

   symtesteq "SMT.Error.IntFair2" (Just ()) $ do
      p <- free_Bool
      guard ((if p then error "SMT.Error.IntFair1 error hit" else 1) == (1 :: Int))

   symtesteq "SMT.Error.CharNoFair" (Just ()) $ do
      p <- free_Bool
      guard p
      guard ((if p then 'a' else error "SMT.Error.CharNoFair error hit") == 'a')

   symtesteq "SMT.Error.CharFair1" (Just ()) $ do
      p <- free_Bool
      guard ((if p then 'a' else error "SMT.Error.CharFair1 error hit") == 'a')

   symtesteq "SMT.Error.CharFair2" (Just ()) $ do
      p <- free_Bool
      guard ((if p then error "SMT.Error.CharFair1 error hit" else 'a') == 'a')

