
{-# LANGUAGE TemplateHaskell #-}

import Prelude hiding (fst, snd, (/=), (==), (<), (>), (&&))
import qualified Prelude

import Seri.Lambda hiding (free, query)
import Seri.SMT.Solver
import Seri.SMT.Yices.Yices2
import Seri.DSEL.DSEL
import Seri.DSEL.SMT
import Seri
import Seri.TH

q1 :: (Solver s) => Query s (Answer Integer)
q1 = do
    x <- free
    assert (x < 6)
    assert (x > 4)
    query x

env :: Env
env = $(loadenvth [seridir] (seridir >>= return . (++ "/Seri/SMT/SMT.sri")))

try :: (Show a) => String -> Query Yices2 a -> IO ()
try nm q = runQuery (RunOptions (Just $ "build/test/DSEL." ++ nm ++ ".dbg") True) env q >>= (putStrLn . show)

main :: IO ()
main = do
    try "q1" q1
    
