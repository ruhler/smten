
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

import Smten.Type
import Smten.ExpH
import Smten.SMT.Query
import Smten.HaskellF.Symbolic
import Smten.HaskellF.Query
import Smten.HaskellF.TH
import qualified Smten.HaskellF.Lib.Prelude as S
import qualified Smten.HaskellF.Lib.SMT as S
import qualified Smten_DSEL as S
import qualified Smten_DSEL
import Smten.SMT.Yices.Yices2    

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
                   (smtenS ((1, 3) :: (Integer, Integer)) :: S.Tuple2__ S.Integer S.Integer)
                   (smtenS ((2, 4) :: (Integer, Integer)) :: S.Tuple2__ S.Integer S.Integer)
    assertS (S.fst x S.== 1)
    query $ realizeS (S.snd x)

data Foo = Bar Integer
         | Sludge Bool
    deriving(Eq, Show)

derive_SmtenT ''Foo
derive_SmtenEH ''Foo
derive_SmtenS ''Foo ''S.Foo

defoo :: S.Foo -> S.Integer
defoo = S.defoo

quserdata :: Query (Answer Foo)
quserdata = do
    f <- qS S.free
    assertS (2 S.== defoo f)
    query $ realizeS f

quserdata2 :: Query (Answer Foo)
quserdata2 = do
    f <- qS S.free
    assertS (f S./= (smtenS (Sludge True)))
    query $ realizeS (f :: S.Foo)

data PolyFoo a = PolyBar a
         | PolySludge Bool
    deriving(Eq, Show)

derive_SmtenT ''PolyFoo
derive_SmtenEH ''PolyFoo
derive_SmtenS ''PolyFoo ''S.PolyFoo

quserdata3 :: Query (Answer (PolyFoo Integer))
quserdata3 = do
    f <- qS S.free
    assertS (f S./= (smtenS (PolyBar True)))
    query $ realizeS (f :: S.PolyFoo S.Bool)

allQ :: (S.Eq a, S.Free a, SmtenS b a) => (a -> S.Bool) -> Query [b]
allQ p = do
    x <- qS S.free
    assertS (p x)
    r <- query $ realizeS x
    case r of
       Satisfiable v -> do
          vs <- allQ (\a -> (p a) S.&& (a S./= smtenS v))
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
    try "share_smten" $ share quadrupleS
    try "qtuple" $ qtuple
    try "quserdata" $ quserdata
    try "quserdata2" $ quserdata2
    try "quserdata3" $ quserdata3
    try "qallQ" $ qallQ
    
