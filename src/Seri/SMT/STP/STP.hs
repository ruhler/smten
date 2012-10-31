
module Seri.SMT.STP.STP (STP(), stp) where

import qualified Seri.Lambda as S
import qualified Seri.SMT.Query as Q
import Seri.SMT.Solver

import qualified Seri.SMT.STP.Concrete as C

data STP = STP

instance Solver STP where
    pretty _ = C.pretty

    initialize = error $ "TODO: STP.initialize"

    run _ cmd = error $ "TODO: STP.run " ++ show cmd
    
    check _ = error $ "TODO: STP.check"

    getIntegerValue _ _ = error $ "STP does not support free Integers"

    getBoolValue _ _ = error $ "TODO: STP.getBoolValue"
    getBitVectorValue _ _ = error $ "TODO: STP.getBitVectorValue"

stp :: Q.Query STP a -> Q.Query STP a
stp = id

