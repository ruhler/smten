
import Prelude hiding ((==), (<), (>))

import Seri.Failable
import Seri.Lambda hiding (free, query)

import Seri.Enoch.Enoch
import Seri.Enoch.Prelude
import Seri.Enoch.SMT
import Seri.SMT.Yices2

q1 :: (Query q) => q (Answer Integer)
q1 = do
    x <- free
    assert (x < 6)
    assert (x > 4)
    query x


incr :: TExp Integer -> TExp Integer
incr x = x + 1

q2 :: (Query q) => q (Answer Integer)
q2 = do
    x <- free
    assert (x < 6)
    assert (incr x > 5)
    query x

quadruple :: TExp Integer -> TExp Integer
quadruple a = a + a + a + a

share :: (Query q) => q (Answer (Integer, Integer))
share = do
    x <- free
    y <- free
    assert (quadruple (x - y) == 24)
    assert (y > 0)
    queryR $ do
      xv <- realize x
      yv <- realize y
      return (xv, yv)

main :: IO ()
main = do
    lib <- load ["src"] "src/Seri/SMT/SMT.sri"
    flat <- attemptIO $ flatten lib
    typed <- attemptIO $ typeinfer (mkEnv flat) flat
    let env = mkEnv typed
    attemptIO $ typecheck env typed

    let try nm q = runYices2 (RunOptions (Just $ "build/src/Seri/Enoch/" ++ nm ++ ".dbg") True) env q >>= (putStrLn . show)

    try "q1" q1
    try "q2" q2
    try "share" share
    
