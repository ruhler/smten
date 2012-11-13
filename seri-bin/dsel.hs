
{-# LANGUAGE TemplateHaskell #-}

import Prelude hiding (fst, snd, (/=), (==), (<), (>), (&&))
import qualified Prelude

import Seri.Lambda hiding (free, query)
import Seri.SMT.Solver (Solver)
import Seri.SMT.Yices.Yices2
import Seri.DSEL.DSEL
import Seri.DSEL.SMT
import Seri
import Seri.TH
import Seri.Type
import Seri.ExpH

q1 :: (Solver s) => Query s (Answer Integer)
q1 = do
    x <- free
    assert (x < 6)
    assert (x > 4)
    query x

incr :: ExpT Integer -> ExpT Integer
incr x = x + 1

q2 :: (Solver s) => Query s (Answer Integer)
q2 = do
    x <- free
    assert (x < 6)
    assert (incr x > 5)
    query x

-- This quadruple inlines the argument completely. The SMT solver doesn't see
-- the sharing between the different instances of 'a'.
quadruple :: ExpT Integer -> ExpT Integer
quadruple a = a + a + a + a

-- This quadruple exposes the sharing to the SMT solver (if sharing is
-- turned on in the elaborator).
quadrupleS :: ExpT Integer -> ExpT Integer
quadrupleS = varET1 "Seri.Tests.DSEL.quadruple"

share :: (Solver s) => (ExpT Integer -> ExpT Integer) -> Query s (Answer (Integer, Integer))
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
    let x = (ite p (seriET (1, 3)) (seriET (2, 4))) :: ExpT (Integer, Integer)
    assert (fst x == 1)
    query (snd x)

data Foo = Bar Integer
         | Sludge Bool
    deriving(Show)

derive_SeriT ''Foo
derive_SeriEH ''Foo

defoo :: ExpT Foo -> ExpT Integer
defoo = varET1 "Seri.Tests.DSEL.defoo"

quserdata :: (Solver s) => Query s (Answer Foo)
quserdata = do
    f <- free
    assert (2 == defoo f)
    query f

allQ :: (Solver s, SeriEH a) => (ExpT a -> ExpT Bool) -> Query s [a]
allQ p = do
    x <- free
    assert (p x)
    r <- query x
    case r of
       Satisfiable v -> do
          vs <- allQ (\a -> (p a) && (a /= seriET v))
          return (v:vs)
       _ -> return []

pred1 :: ExpT Integer -> ExpT Bool
pred1 x = (x > 3) && (x < 6)

qallQ :: (Solver s) => Query s [Integer]
qallQ = allQ pred1

env :: Env
env = $(loadenvth [seridir] (seridir >>= return . (++ "/Seri/Tests/DSEL.sri")))

try :: (Show a) => String -> Query Yices2 a -> IO ()
try nm q = runQuery (RunOptions (Just $ "build/test/DSEL." ++ nm ++ ".dbg")) env q >>= (putStrLn . show)

main :: IO ()
main = do
    try "q1" q1
    try "q2" q2
    try "share_haskell" $ share quadruple
    try "share_seri" $ share quadrupleS
    try "qtuple" $ qtuple
    try "quserdata" $ quserdata
    try "qallQ" $ qallQ
    
