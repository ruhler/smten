
{-# LANGUAGE RebindableSyntax, NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Smten.Tests.Basic (tests) where

import Smten.Prelude
import Smten.Tests.Test
import Smten.Tests.Test -- test duplicate import

------ This is a comment with lots of dashes

{- This is 
 - a multiline comment
 -  {- with a nested comment inside! -}
 -}

foo1 :: Integer
foo1 = (\x -> x*x + 3*x + 2) 5

foo2 :: Integer -> Integer
foo2 x = x*x + 3*x + 2

rfact :: Integer -> Integer
rfact x = if (x < 1) then 1 else x * rfact (x-1)

data MaybeInteger = NoInteger | JustInteger Integer
   -- deriving (Show)

fromMaybeInteger :: Integer -> MaybeInteger -> Integer
fromMaybeInteger def = \mi ->
    case mi of
        JustInteger i -> i
        NoInteger -> def

fromMaybeBool :: Bool -> Maybe Bool -> Bool
fromMaybeBool def = \mb ->
    case mb of
        Just b -> b
        Nothing -> def

multiclause :: Integer -> Integer
multiclause x | True <- x == 2 = 10
multiclause x | True <- x == 3 = 20
multiclause x | True <- x == 4 = 30
multiclause x | True <- x == 5 = 40
multiclause _ = 50

tupleswap :: (a, b) -> (b, a)
tupleswap (x, y) = (y, x)

listswaptop :: [a] -> [a]
listswaptop (x:xs) = (head xs) : (x : (tail xs))

listdifftop :: [Integer] -> Integer
listdifftop (x:y:_) = x - y

sum2 :: Integer -> Integer -> Integer
sum2 a b = a + b

sum3 :: Integer -> Integer -> Integer -> Integer
sum3 a b c = (a + b) + c

unary2int :: [()] -> Integer
unary2int [] = 0
unary2int (_:xs) = 1 + unary2int xs

class Foo a where
    foo :: a -> Integer

instance Foo Bool where
    foo _ = 1

instance Foo Integer where
    foo _ = 2

foofun :: (Foo a) => a -> Integer
foofun x = foo x * foo x + 3*(foo x) + foo True

class (Foo a) => FooBar a where
    bar :: a -> a -> Integer

instance FooBar Bool where
    bar a b = foo a + foo b

instance FooBar Integer where
    bar a b = foo a - foo b

foobarfun :: (FooBar a) => a -> Integer
foobarfun x = foo x + bar x x 

data Apple = Apple {
    isgreen :: Bool,
    numseeds :: Integer
}

data Wonderful a = Wonderful {
    thething :: a,
    howgreat :: Integer
}

thegreatthing :: Wonderful Integer -> Integer
thegreatthing w = thething w + howgreat w

-- record type construction with no fields
data EmptyRecord = EmptyRecord {}

class MultiFoo a b where
    multifoo :: a -> b -> Integer

instance MultiFoo Bool Bool where
    multifoo _ _ = 1

instance MultiFoo Integer Integer where
    multifoo _ _ = 2

instance MultiFoo Bool Integer where
    multifoo _ _ = 3

instance MultiFoo Integer Bool where
    multifoo _ _ = 4

-- Verify the type checker looks at contexts of method signatures for
-- verifying all contexts are satisfied
class CheckCtx a where
    checkctx :: (Monad m) => a -> m a

instance CheckCtx Bool where
    checkctx x = return x


unused :: a -> Integer
unused x = 42

multifoofun :: (MultiFoo a b) => a -> b -> Integer
multifoofun x y = (((multifoo x y) * (multifoo x y))
                    + (3 * (multifoo x y))) + (multifoo True False)

-- shadow x = 2*(x+1)
shadow :: Integer -> Integer
shadow = \x -> (\x -> 2*x) (x+1)

caseshadow :: Integer -> Integer
caseshadow = \x -> (case 2*x of { x -> x; })

funnydesugar :: Bool
funnydesugar = let { x = null "a" } in x

thedo :: Maybe Bool -> Maybe Integer
thedo m = do
    b <- m
    if b then return 5 else Nothing

notand :: Bool
notand = True && False

testlet :: Integer
testlet = 
  let fudge = 1 + 2
      bar = fudge + 3
  in bar + bar

-- Verify layout inserts a '}' if needed on parse error.
testcloseerr :: Integer
testcloseerr = let x = 5 in x

shadowmeth :: Integer
shadowmeth = let { foo = 3; } in foo

parsenotctx :: (Maybe Integer -> Bool) -> Bool
parsenotctx = undefined

patternlet :: Integer
patternlet = 
  let (a, b) = (1, 5)
  in a + b

--data DerivedEq a = DEq1 a | DEq2 a a
--    deriving (Eq)

-- Funny operator, for testing precedence of operaters with no fixity
-- specified.
(++++) :: Integer -> Integer -> Integer
(++++) a b = 2*a + b

tests :: IO ()
tests = do
    test "a" ((42 :: Integer) == (\x -> (((x*x) + (3*x)) + 2)) 5)
    test "b" (42 == (foo1))
    test "c" (42 == (foo2 5))
    test "d" (True)
    test "e" ((23 :: Integer) == (if 6 < (4 :: Integer) then 42 else 23 ))
    test "f" ((5 :: Integer) == ((id id) 5))
    test "g" (5 == (fromMaybeInteger 10 (JustInteger 5)))
    test "h" (10 == (fromMaybeInteger 10 NoInteger))
    test "i" (fromMaybeBool False (Just True))
    test "j" (Just (18 :: Integer) == Just 18)
    test "k" (Just (18 :: Integer) /= Just 16)
    test "l" (Just (18 :: Integer) /= Nothing)
    test "m" ((30 :: Integer) == (case ((1+3) :: Integer) of
                                       x | True <- x == 2 -> 10
                                       x | True <- x == 3 -> 20
                                       x | True <- x == 4 -> 30
                                       x | True <- x == 5 -> 40
                                       _ -> 50
                                 ))
    test "n" (30 == (multiclause 4))
    test "o" (30 == (snd (tupleswap (30 :: Integer, 40 :: Integer))))
    test "p" ((20 :: Integer) == (listdifftop (listswaptop [10, 30, 50, 0])))
    test "q" (12 == (sum2 5 7))
    test "r" (20 == (sum3 5 7 8))
    test "s" (3 == (unary2int [(), (), ()]))
    test "t" (1 == (foo True))
    test "u" (2 == (foo (42 :: Integer)))
    test "v" (5 == (foofun False))
    test "w" (11 == (foofun (5 :: Integer)))
    test "x" (11 == (numseeds (Apple True 11)))
    test "y" (11 == (numseeds (Apple { numseeds = 11, isgreen = False })))
    test "z" (8 == (numseeds ((Apple True 11) { numseeds = 8 })))
    test "A" ((11 :: Integer) == (thething (Wonderful 11 32)))
    test "B" (42 == (unused True))
    test "C" (19 == (multifoofun True (12 :: Integer)))
    test "D" (8 == (shadow 3))
    test "E" (6 == (caseshadow 3))
    test "F" (12 == testlet)
    test "G" (3 == shadowmeth)
    test "H" (3 == (length [1 :: Integer, 5, 2]))
    test "I" (and [True, True, True])
    test "J" ('A' == 'A')
    test "K" ('A' /= 'B')
    test "L" (6 == patternlet)
    --test "M" (DEq2 (5 :: Integer) 9 == DEq2 5 9)
    --test "N" (DEq1 (5 :: Integer) /= DEq2 5 9)
    test "P" (length "foo\n" == 4)
    test "hex literal" (0x18 == (24 :: Integer))
    test "HEX literal" (0X18 == (24 :: Integer))
    test "or lazy" (True || (error "or is not lazy"))
    test "and lazy" (not (False && (error "and is not lazy")))
    test "precedence" (1*2+3-4*5+6-7 == negate (16 :: Integer))
    test "precedence 2" ((let f = (-) 
                          in 10 `f` 2 `f` 3
                         ) == (5 :: Integer))
    test "precedence 3" ((2 ++++ 3 ++++ 5) == (19 :: Integer))
    testaspattern
    testarithsequence
    teststringliteralpattern
    testletclause
    testlistcomprehension
    testdopattern
    testlambda
    testvarop
    testpatternguard
    testtypesyn
    testwhereclause
    testclassctx
    --testsaderiving
    testirrefutable
    testletrec

    putStrLn "Basic PASSED"

--data FunnyDeriveEQ a = FunnyDeriveEQ (a, a)
--    deriving (Eq)
--
--data SDerivedEq a = SDEq1 a | SDEq2 a a
--deriving instance (Eq a) => Eq (SDerivedEq a)

--testsaderiving :: IO ()
--testsaderiving = do
--    test "saderive1" (SDEq2 (5 :: Integer) 9 == SDEq2 5 9)
--    test "saderive2" (SDEq1 (5 :: Integer) /= SDEq2 5 9)

testclassctx :: IO ()
testclassctx = do
    test "class ctx" (foobarfun True == 3)

type AMultiArgSynonym a = (a, a)

fsyn :: AMultiArgSynonym Integer -> Integer
fsyn (a, b) = a + b

testtypesyn :: IO ()
testtypesyn = do
    test "type syn" (fsyn (1, 2) == 3)


fpatgd :: (Integer, Integer) -> Integer
fpatgd p = case p of
             (x, _) | True <- x == 1 -> 0
                    | True <- x > 3 -> 1
             _ | (x, y) <- p, True <- y == 2 -> x
             _ -> 3

fpatgd2 :: (Integer, Integer) -> Integer
fpatgd2 (x, _)
  | True <- x == 1 = 0
  | True <- x > 3 = 1
fpatgd2 p
  | (x, y) <- p, True <- y == 2 = x
fpatgd2 _ = 3

testpatternguard :: IO ()
testpatternguard = do
    test "case pattern guard" ([0, 1, 2, 3] == map fpatgd [(1, 4), (4, 3), (2, 2), (3, 7)])
    test "function pattern guard" ([0, 1, 2, 3] == map fpatgd2 [(1, 4), (4, 3), (2, 2), (3, 7)])

testvarop :: IO ()
testvarop = do
    test "var op" (4 `elem` [1, 4, 2 :: Integer])

testlambda :: IO ()
testlambda = do
    test "simple lambda" ((\a -> a || False) True)
    test "patmatch lambda" ((\(_, a) -> a) (False, True))
    test "multi lambda" ((\a b -> a || b) False True)

testdopattern :: IO ()
testdopattern = do
    Just b <- return (Just True)
    test "do pattern" b

testaspattern :: IO ()
testaspattern = do
    test "as pattern" (
        case [1, 2, 3] of
          v@(x:xs) -> (length v + x + length xs) == 6
          _ -> False
      )

wheref :: Integer -> Integer -> Integer
wheref x y = 1 + z
  where z = x + y 

testwhereclause :: IO ()
testwhereclause = do
    test "where clause f" (wheref 1 2 == 4)
    test "where clause c" (
        case Just True of
            Just v | False <- nv -> True   
               where nv = not v
            _ -> False
      )

    test "where clause shadow" (
        3 == (let z = (3 :: Integer)
              in case z of
                    p | True <- p /= 3 -> z
                      where z = p+1
                    _ -> z
             )
      )

testletclause :: IO ()
testletclause = do
    test "let clause" (
        let foo (a, True) = a
            foo (a, False) = not a
        in foo (False, False)
     )
    test "let clause2" (
        let foo a True = a
            foo a False = not a
        in foo False False
     )

testarithsequence :: IO ()
testarithsequence = do
    test "fromto" ([1..5] == [1,2,3,4,5 :: Integer])
    test "from" (take 4 [1..] == [1, 2, 3, 4 :: Integer])
    test "fromthento" ([1, 3 .. 9] == [1,3,5,7,9 :: Integer])
    test "fromthen" (take 4 [1, 3 ..] == [1,3,5,7 :: Integer])

testlistcomprehension :: IO ()
testlistcomprehension = do
    test "lcomp" (
      let l = [a | (a, _) <- [(1, 2), (3, 4), (5 :: Integer, 6 :: Integer)]]
      in [1, 3, 5] == l
     )

testirrefutable :: IO ()
testirrefutable = do
    test "irref 1" (case (error "irref 1 arg" :: (Bool, Bool)) of
                        ~(x, y) -> True
     )
    test "irref 2" (case (True, False) of
                      ~(x, y) -> x && not y
     )
    test "irref 3" (case [(False, True), error "irref 3 arg"] of
                      ~[x, ~(a, b)] -> not (fst x) && snd x
     )

testletrec :: IO ()
testletrec = do
    test "letrec 1" ( (3 :: Integer, 5 :: Integer) == 
        let (x, y) = (3, x+2)
        in (x, y)
      )
    test "letrec 2" ( (120 :: Integer) == 
        (let f = \x -> if x < 2 then 1 else x * f (x - 1)
         in f 5)
      )
    test "letrec 3" ( (3 :: Integer, 5 :: Integer) ==
        let y = x+2
            x = 3
        in (x, y)
      )
    

teststringliteralpattern :: IO ()
teststringliteralpattern = do
    test "stringliteralpattern" (
        case "foo" of
          "bar" -> False
          "foo" -> True
      )

testparsedot :: IO ()
testparsedot = do
   putStrLn . show $ "parsehi!"

