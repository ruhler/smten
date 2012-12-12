
{-# LANGUAGE TemplateHaskell #-}

import Seri.Type
import Seri.ExpH
import Seri.SMT.Query
import Seri.HaskellF.Symbolic
import Seri.HaskellF.Query
import qualified Seri.HaskellF.Lib.Prelude as S
import qualified Seri.HaskellF.Lib.SMT as S
import qualified Seri_DSEL as S
import Seri.SMT.Yices.Yices2    

q1 :: Query (Answer Integer)
q1 = do
    x <- qS S.free
    assertS (x S.< 6)
    assertS (x S.> 4)
    query $ realizeS (x :: S.Integer)

incr :: S.Integer -> S.Integer
incr x = x S.+ 1

q2 :: Query (Answer Integer)
q2 = do
    x <- qS S.free
    assertS (x S.< 6)
    assertS (incr x S.> 5)
    query $ realizeS x

-- This quadruple inlines the argument completely. The SMT solver doesn't see
-- the sharing between the different instances of 'a'.
quadruple :: S.Integer -> S.Integer
quadruple a = a S.+ a S.+ a S.+ a

-- This quadruple exposes the sharing to the SMT solver (if sharing is
-- turned on in the elaborator).
quadrupleS :: S.Integer -> S.Integer
quadrupleS = S.quadruple

share :: (S.Integer -> S.Integer) -> Query (Answer (Integer, Integer))
share f = do
    x <- qS S.free
    y <- qS S.free
    assertS (f (x S.- y) S.== 24)
    assertS (y S.> 0)
    query $ do
      xv <- realizeS x
      yv <- realizeS y
      return (xv, yv)

qtuple :: Query (Answer Integer)
qtuple = do
    p <- qS S.free
    let x = S.__caseTrue p
                   (S.__mkTuple2__ (1 :: S.Integer) (3 :: S.Integer))
                   (S.__mkTuple2__ (2 :: S.Integer) (4 :: S.Integer))
    assertS (S.fst x S.== 1)
    query $ realizeS (S.snd x)

data Foo = Bar Integer
         | Sludge Bool
    deriving(Show)

derive_SeriT ''Foo
derive_SeriEH ''Foo

defoo :: S.Foo -> S.Integer
defoo = S.defoo

quserdata :: Query (Answer Foo)
quserdata = do
    f <- qS S.free
    assertS (2 S.== defoo f)
    query $ realizeS f

allQ :: (S.Eq a, S.Free a, Symbolic a, SeriEH b) => (a -> S.Bool) -> Query [b]
allQ p = do
    x <- qS S.free
    assertS (p x)
    r <- query $ realizeS x
    case r of
       Satisfiable v -> do
          vs <- allQ (\a -> (p a) S.&& (a S./= seriS v))
          return (v:vs)
       _ -> return []

pred1 :: S.Integer -> S.Bool
pred1 x = (x S.> 3) S.&& (x S.< 6)

qallQ :: Query [Integer]
qallQ = allQ pred1

try :: (Show a) => String -> Query a -> IO ()
try nm q = do
    y <- yices2
    r <- runQuery (RunOptions (Just $ "build/test/DSEL." ++ nm ++ ".dbg") y) q
    putStrLn $ show r

main :: IO ()
main = do
    try "q1" q1
    try "q2" q2
    try "share_haskell" $ share quadruple
    try "share_seri" $ share quadrupleS
    try "qtuple" $ qtuple
    try "quserdata" $ quserdata
    try "qallQ" $ qallQ
    
