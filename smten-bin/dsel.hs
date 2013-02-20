
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Functor((<$>))
import Data.Maybe(fromMaybe)

import Smten.Type
import Smten.ExpH
import Smten.SMT.Symbolic
import Smten.SMT.SMT
import Smten.HaskellF.HaskellF
import Smten.HaskellF.TH
import qualified Smten.HaskellF.Lib.Prelude as S
import qualified Smten.HaskellF.Lib.Symbolic as S
import qualified Smten_DSEL as S
import qualified Smten_DSEL
import Smten.SMT.Yices.Yices2    

test :: (Monad m) => String -> Bool -> m ()
test msg p = if p then return () else error msg

symbolicHF :: (HaskellF a) => S.Symbolic a -> Symbolic a
symbolicHF s 
 | Just v <- de_symbolicEH (unbox s) = box <$> v
 | otherwise = error "symbolicHF failed"

freeHF :: (S.Free a, HaskellF a) => Symbolic a
freeHF = symbolicHF S.free

assertHF :: S.Bool -> Symbolic ()
assertHF x = symbolicHF (S.assert x) >> return ()

queryHF :: (HaskellF a) => Symbolic a -> SMT (Maybe a)
queryHF s = do
    r <- query (realize . unbox <$> s)
    return (box <$> r)

queryHFC :: (SmtenHF c f) => Symbolic f -> SMT (Maybe c)
queryHFC s = do
    r <- queryHF s
    return (fromMaybe (error "queryHFC") . de_smtenHF <$> r)

q1 :: SMT ()
q1 = do
    r <- queryHFC $ do
        x <- freeHF
        assertHF (x S.< 6)
        assertHF (x S.> 4)
        return (x :: S.Integer)
    test "q1" (r == Just (5 :: Integer))

qall :: SMT ()
qall = do
    q1

main :: IO ()
main = do
    s <- yices2
    runSMT (RunOptions (Just "build/test/DSEL.yices2.dbg") s) qall
    putStrLn "DSEL PASSED"
    
