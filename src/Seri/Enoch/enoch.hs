
{-# LANGUAGE TemplateHaskell #-}

import Prelude hiding (fst, snd, (/=), (==), (<), (>), (&&))
import qualified Prelude

import Data.Functor

import Seri.Failable
import Seri.Lambda hiding (free, query)

import Seri.Enoch.Enoch
import Seri.Enoch.EnochTH
import Seri.Enoch.Prelude
import Seri.Enoch.SMT
import Seri.SMT.Solver (Solver)
import Seri.SMT.Yices2

q1 :: (Solver s) => Query s (Answer Integer)
q1 = do
    x <- free
    assert (x < 6)
    assert (x > 4)
    query x


incr :: TExp Integer -> TExp Integer
incr x = x + 1

q2 :: (Solver s) => Query s (Answer Integer)
q2 = do
    x <- free
    assert (x < 6)
    assert (incr x > 5)
    query x

-- This quadruple inlines the argument completely. The SMT solver doesn't see
-- the sharing between the different instances of 'a'.
quadruple :: TExp Integer -> TExp Integer
quadruple a = a + a + a + a

-- This quadruple exposes the sharing to the SMT solver (if sharing is
-- turned on in the elaborator).
quadrupleS :: TExp Integer -> TExp Integer
quadrupleS = varE1 "Seri.Enoch.Enoch.quadruple"

share :: (Solver s) => (TExp Integer -> TExp Integer) -> Query s (Answer (Integer, Integer))
share f = do
    x <- free
    y <- free
    assert (f (x - y) == 24)
    assert (y > 0)
    queryR $ do
      xv <- realize x
      yv <- realize y
      return (xv, yv)

qtuple :: (Solver s) => Query s (Answer Integer)
qtuple = do
    p <- free
    let x = (ite p (pack (1, 3)) (pack (2, 4))) :: TExp (Integer, Integer)
    assert (fst x == 1)
    query (snd x)

data Foo = Bar Integer
         | Sludge Bool
    deriving(Show)

derive_SeriableT ''Foo
derive_SeriableE ''Foo

defoo :: TExp Foo -> TExp Integer
defoo = varE1 "Seri.Enoch.Enoch.defoo"

quserdata :: (Solver s) => Query s (Answer Foo)
quserdata = do
    f <- free
    assert (2 == defoo f)
    query f

allQ :: (Solver s, SeriableE a) => (TExp a -> TExp Bool) -> Query s [a]
allQ p = do
    x <- free
    assert (p x)
    r <- query x
    case r of
       Satisfiable v -> do
          vs <- allQ (\a -> (p a) && (a /= pack v))
          return (v:vs)
       _ -> return []

pred1 :: TExp Integer -> TExp Bool
pred1 x = (x > 3) && (x < 6)

qallQ :: (Solver s) => Query s [Integer]
qallQ = allQ pred1

main :: IO ()
main = do
    lib <- load ["src"] "src/Seri/Enoch/Enoch.sri"
    flat <- attemptIO $ flatten lib
    typed <- attemptIO $ typeinfer (mkEnv flat) flat
    let env = mkEnv typed
    attemptIO $ typecheck env typed

    let try nm q = runYices2 (RunOptions (Just $ "build/src/Seri/Enoch/" ++ nm ++ ".dbg") True) env q >>= (putStrLn . show)

    try "q1" q1
    try "q2" q2
    try "share_haskell" $ share quadruple
    try "share_seri" $ share quadrupleS
    try "qtuple" $ qtuple
    try "quserdata" $ quserdata
    try "qallQ" $ qallQ
    
