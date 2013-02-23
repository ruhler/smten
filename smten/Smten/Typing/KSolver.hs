module Smten.Typing.KSolver (ksolve) where

import Data.Functor((<$>))
import qualified Data.Map as Map

import Smten.Name
import Smten.Type
import Smten.Failable
import Smten.Ppr
import Smten.Typing.Solver

-- We solver kind constraints by converting them to type constraints, solving
-- the type constraints, and converting back to kinds.

ksolve :: [(Kind, Kind)] -> Failable (Map.Map Name Kind)
ksolve xs = Map.map t2k <$> solve [(k2t a, k2t b) | (a, b) <- xs]

k2t :: Kind -> Type
k2t StarK = ConT (name "StarK")
k2t NumK = ConT (name "NumK")
k2t (ArrowK a b) = AppT (AppT (ConT (name "ArrowK")) (k2t a)) (k2t b)
k2t (VarK n) = VarT n UnknownK
k2t UnknownK = UnknownT

t2k :: Type -> Kind
t2k (ConT n)
  | n == name "StarK" = StarK
  | n == name "NumK" = NumK
t2k (AppT (AppT (ConT n) a) b)
  | n == name "ArrowK" = ArrowK (t2k a) (t2k b)
t2k (VarT n _) = VarK n
t2k UnknownT = UnknownK
t2k t = error $ "t2k: " ++ pretty t

