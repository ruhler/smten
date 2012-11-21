
module Seri.ExpH.Utils () where

import Seri.Type
import Seri.ExpH.ExpH
import Seri.Sig

instance Assign ExpH where
   assignl f e =
    let me = assignl f 
        mt = assignl f
    in case e of
         LitEH {} -> e
         ConEH (Sig n t) -> ConEH (Sig n (mt t))
         VarEH (Sig n t) -> VarEH (Sig n (mt t))
         AppEH m a b -> AppEH m (me a) (me b)
         LamEH m (Sig n t) b -> LamEH m (Sig n (mt t)) $ \x -> (me (b x))
         CaseEH m x (Sig kn kt) y n -> CaseEH m (me x) (Sig kn (mt kt)) (me y) (me n)

