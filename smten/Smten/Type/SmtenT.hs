
{-# LANGUAGE FlexibleInstances #-}

-- | Relating haskell types to smten types.
module Smten.Type.SmtenT (
    SmtenT(..), SmtenT1(..), SmtenT2(..), SmtenT3(..), SmtenT4(..),
 ) where

import Smten.Name
import Smten.Type.Type
import Smten.Type.Sugar
   
-- | Class of haskell types which have a corresponding smten type.
class SmtenT a where
    -- | The smten type corresponding to the type 'a'.
    -- The argument is ignored.
    smtenT :: a -> Type

-- | Class of unary type constructors having a corresponding smten type
-- constructor.
class SmtenT1 m where
    -- | The smten unary type constructor corresponding to the type constructor
    -- 'm'. The argument is ignored.
    smtenT1 :: m a -> Type

class SmtenT2 m where
    smtenT2 :: m a b -> Type

class SmtenT3 m where
    smtenT3 :: m a b c -> Type

class SmtenT4 m where
    smtenT4 :: m a b c d -> Type

instance (SmtenT1 m, SmtenT a) => SmtenT (m a) where
    smtenT x =
      let t :: m a -> a
          t _ = undefined
      in appT (smtenT1 x) (smtenT (t x))


instance (SmtenT2 m, SmtenT a) => SmtenT1 (m a) where
    smtenT1 x =
      let t :: m a b -> a
          t _ = undefined
      in appT (smtenT2 x) (smtenT (t x))

instance (SmtenT3 m, SmtenT a) => SmtenT2 (m a) where
    smtenT2 x =
      let t :: m a b c -> a
          t _ = undefined
      in appT (smtenT3 x) (smtenT (t x))

instance (SmtenT4 m, SmtenT a) => SmtenT3 (m a) where
    smtenT3 x =
      let t :: m a b c d -> a
          t _ = undefined
      in appT (smtenT4 x) (smtenT (t x))


instance SmtenT () where
    smtenT _ = unitT

instance SmtenT Char where
    smtenT _ = charT

instance SmtenT Integer where
    smtenT _ = integerT

instance SmtenT Bool where
    smtenT _ = boolT

instance SmtenT1 IO where
    smtenT1 _ = conT ioN

instance SmtenT2 (->) where
    smtenT2 _ = conT arrowN

instance SmtenT2 (,) where
    smtenT2 _ = conT (tupleN 2)

