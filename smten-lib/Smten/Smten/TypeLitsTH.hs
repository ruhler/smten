
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Smten.Smten.TypeLitsTH (TAdd, mksuccs, mkadds) where

import Language.Haskell.TH.Syntax
import GHC.TypeLits
import Data.Functor

type family TAdd (m :: Nat) (n :: Nat) :: Nat

type instance TAdd 0 x = x

mksuccs :: [Integer] -> Q [Dec]
mksuccs xs = concat <$> mapM mksucc xs

mkadds :: [Integer] -> Q [Dec]
mkadds xs = concat <$> mapM mkadd xs

-- type instance 1 + N = (N+1)
--
-- For example: mksucc 2 gives:
--  type instance 1 + 2 = 3
mksucc :: Integer -> Q [Dec]
mksucc n = do
  let one = LitT (NumTyLit 1)
      nty = LitT (NumTyLit n)
      ans = LitT (NumTyLit (n+1))
  return [TySynInstD ''TAdd [one, nty] ans]
  

--  type instance N + b = (N-1) + (1 + b)
--
--  For example: mkadd 5 gives:
--  type instance 5 + b = 4 + (1 + b)
mkadd :: Integer -> Q [Dec]
mkadd n = do
  let one = LitT (NumTyLit 1)
      nty = LitT (NumTyLit n)
      bty = VarT (mkName "b")
      bp1 = AppT (AppT (ConT ''TAdd) one) bty
      ans = AppT (AppT (ConT ''TAdd) (LitT (NumTyLit (n-1)))) bp1
  return [TySynInstD ''TAdd [nty, bty] ans]
      
