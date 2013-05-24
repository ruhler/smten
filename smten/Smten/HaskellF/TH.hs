
{-# LANGUAGE TemplateHaskell #-}

module Smten.HaskellF.TH (haskellf_Data) where

import Data.Maybe(fromMaybe)
import Data.Functor((<$>))

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Smten.Failable
import qualified Smten.Name as S
import qualified Smten.Dec as S
import Smten.HaskellF.HaskellF
import Smten.HaskellF.Compile

haskellf_Data :: S.Name -> [S.TyVar] -> [S.Con] -> Q [Dec]
haskellf_Data n tyvars constrs = attemptM $ hfData n tyvars constrs

