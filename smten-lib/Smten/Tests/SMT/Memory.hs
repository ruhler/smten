
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.Tests.SMT.Memory (memtest) where

import Smten.Prelude
import Smten.Symbolic
import Smten.Tests.SMT.Test

memtest :: SMTTest ()
memtest = memtest' 0

memtest' :: Integer -> SMTTest ()
memtest' n = do

   let p x y z =
           if x && y
               then not z
               else x == (y || z)

       tstcmplx (Just (jv, kv, lv)) = p jv kv lv
       tstcmplx _ = False

   symtest ("SMT.Memtest.Complex." ++ show n) tstcmplx $ do
       j <- free_Bool
       k <- free_Bool
       l <- free_Bool
       assert (p j k l)
       return (j, k, l)

   memtest' (n+1)

