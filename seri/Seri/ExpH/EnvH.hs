
-- | An environment for fast lookup of variables in elaboration.
module Seri.ExpH.EnvH (
    EnvH(), mkEnvH, onEnv,
    ) where

import Debug.Trace

import Control.Monad
import Data.Either
import Data.List(nub)
import qualified Data.Map as Map

import Data.IORef
import System.IO.Unsafe

import Seri.Name
import Seri.Sig
import Seri.Failable
import Seri.Type
import Seri.Dec
import Seri.Ppr
import Seri.Exp
import Seri.ExpH.ExpH
import Seri.ExpH.ToExpH
import Seri.ExpH.Utils

type EnvH = Env

mkEnvH :: Env -> EnvH
mkEnvH = id

onEnv :: (Env -> a) -> EnvH -> a
onEnv = id

