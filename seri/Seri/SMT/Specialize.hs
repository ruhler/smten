
module Seri.SMT.Specialize (
    Logic(..), core,
    specialize,
    ) where

import Seri.ExpH

data Logic = Logic {
    th_integer :: Bool,
    th_bit :: Bool
}

core :: Logic
core = Logic {
    th_integer = False,
    th_bit = False
  }
    
-- Specialize an expression for a given logic.
-- TODO: actually perform specialization.
specialize :: Logic -> ExpH -> ExpH
specialize _ = id

