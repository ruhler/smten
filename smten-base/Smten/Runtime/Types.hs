
{-# LANGUAGE PatternGuards #-}

module Smten.Runtime.Types (
    Type(..), Any(..),
    ErrorString(..), errstr, doerr,
    Model, model, m_cached, lookupBool,
    Bool(..), andB, notB, iteB,
    ) where

import Prelude hiding (Bool(..))

import System.IO.Unsafe
import qualified Smten.Runtime.AnyMap as A
import Smten.Runtime.FreeID

data Type = BoolT
    deriving (Show)

data Any = BoolA Bool

data ErrorString =
   ErrorString String
 | ErrorString_Ite Bool ErrorString ErrorString

errstr :: String -> ErrorString
errstr = ErrorString

doerr :: ErrorString -> a
doerr (ErrorString msg) = error $ "smten user error: " ++ msg


data Model = Model {
    m_vars :: [(FreeID, Any)],
    m_cache :: A.AnyMap
}

model :: [(FreeID, Any)] -> IO Model
model vars = do
   cache <- A.new
   return (Model vars cache)

-- lookup the value of an object under the given model.
-- The lookup is memoized.
m_cached :: Model -> (Model -> a -> b) -> a -> b
m_cached m f x = unsafeDupablePerformIO $ do
    let mc = m_cache m
    xfnd <- A.lookup mc x
    case xfnd of
       Just v -> return v
       Nothing -> do
         let v = f m x
         A.insert mc x v
         return v

lookupBool :: Model -> FreeID -> Bool
lookupBool m nm
  | Just (BoolA x) <- lookup nm (m_vars m) = x
  | otherwise = error "lookupBool failed"

data Bool =
     True
   | False
   | Bool_Ite Bool Bool Bool
   | Bool_And Bool Bool
   | Bool_Not Bool
   | Bool_Var FreeID
   | Bool_Err ErrorString
   | Bool_Prim (Model -> Bool) Bool

andB :: Bool -> Bool -> Bool
andB True x = x
andB False x = False
andB a True = a
andB a False = False
andB a b = Bool_And a b

notB :: Bool -> Bool
notB True = False
notB False = True
notB (Bool_Not x) = x
notB x = Bool_Not x

iteB :: Bool -> Bool -> Bool -> Bool
iteB True x _ = x
iteB False _ x = x
iteB (Bool_Not x) a b = iteB x b a
iteB p True False = p
iteB p False True = notB p
iteB p a b = Bool_Ite p a b

