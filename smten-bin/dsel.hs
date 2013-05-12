
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Functor((<$>))
import Data.Maybe(fromMaybe)

import Smten.Type
import Smten.ExpH
import Smten.SMT.SMT
import Smten.HaskellF.HaskellF
import Smten.HaskellF.TH
import Smten.HaskellF.SMT
import qualified Smten.HaskellF.Lib.Prelude as S
import qualified Smten.HaskellF.Lib.Symbolic as S
import qualified Smten.Lib.Prelude as S
import qualified Smten.Lib.Smten.SMT.Symbolic as S
import qualified Smten.Lib.Smten.Tests.DSEL as S
import qualified Smten.Lib.Smten.Tests.DSEL
import Smten.SMT.Yices.Yices2    

test :: (Monad m) => String -> Bool -> m ()
test msg p = if p then return () else error msg

freeHF :: (S.Free a, HaskellF a) => Symbolic a
freeHF = symbolicHF S.free

q1 :: SMT ()
q1 = do
    r <- queryHFC $ do
        x <- freeHF
        assertHF (x S.< 6)
        assertHF (x S.> 4)
        return (x :: S.Integer)
    test "q1" (r == Just (5 :: Integer))

incr :: S.Integer -> S.Integer
incr x = x S.+ 1

q2 :: SMT ()
q2 = do
    r <- queryHFC $ do
        x <- freeHF
        assertHF (x S.< 6)
        assertHF (incr x S.> 5)
        return (x :: S.Integer)
    test "q2" (r == Just (5 :: Integer))


qall :: SMT ()
qall = do
    q1
    q2

main :: IO ()
main = do
    s <- yices2
    runSMT s qall
    putStrLn "DSEL PASSED"
    
