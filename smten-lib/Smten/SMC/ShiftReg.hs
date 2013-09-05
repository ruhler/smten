
{-# LANGUAGE NoImplicitPrelude, RebindableSyntax #-}
module Smten.SMC.ShiftReg (shiftregm, shiftregf) where

import Smten.Prelude
import Smten.Symbolic
import Smten.SMC.SMC

data SRState = SRState {
    x0 :: Bool,
    x1 :: Bool,
    x2 :: Bool
}

instance Free SRState where
    free = do
        a <- free
        b <- free
        c <- free
        return (SRState a b c)

instance Show SRState where
    show x = "SRState { "
        ++ "x0 = " ++ show (x0 x) ++ ", "
        ++ "x1 = " ++ show (x1 x) ++ ", "
        ++ "x2 = " ++ show (x2 x) ++ "}"

srtrans_bad :: SRState -> SRState -> Bool
srtrans_bad a b = (x0 b == x1 a) && (x1 b == x2 a) && (x2 b == True)

srtrans_good :: SRState -> SRState -> Bool
srtrans_good a b = (x0 b == x1 a) && (x1 b == x2 a) && (x2 b == False)

srinit :: SRState -> Bool
srinit = const True

shiftregm :: Model SRState
shiftregm = Model { _I = srinit, _T = srtrans_bad }

allzero :: SRState -> Bool
allzero s = not (x0 s) && not (x1 s) && not (x2 s)

shiftregf :: LTL SRState
shiftregf = G (P (not . allzero))

