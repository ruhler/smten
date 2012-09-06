
import Prelude hiding ((<), (>))

import Seri.Failable
import Seri.Lambda hiding (free, query)

import Seri.Enoch.Enoch
import Seri.Enoch.Prelude
import Seri.Enoch.SMT

q1 :: Query (Answer Integer)
q1 = do
    x <- free
    assert (x < 6)
    assert (x > 4)
    query x

main :: IO ()
main = do
    lib <- load ["src"] "src/Seri/SMT/SMT.sri"
    flat <- attemptIO $ flatten lib
    typed <- attemptIO $ typeinfer (mkEnv flat) flat
    let env = mkEnv typed
    attemptIO $ typecheck env typed

    runQuery (RunOptions (Just "build/src/Seri/Enoch/q1.dbg") True) env q1 >>= (putStrLn . show)
    
