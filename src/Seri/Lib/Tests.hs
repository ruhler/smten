
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Tests where

import Seri
import Seri.Lib.Prelude

[s|
    foo1 :: Integer
    foo1 = (\x -> x*x+3*x+2) 5

    foo2 :: Integer -> Integer
    foo2 x = x*x+3*x+2

    rfact :: Integer -> Integer
    rfact x = if (x < 1) then 1 else x * rfact (x-1)

    id :: a -> a
    id x = x
|]


[s|
    data MaybeInteger = NoInteger | JustInteger Integer

    fromMaybeInteger :: Integer -> MaybeInteger -> Integer
    fromMaybeInteger def = \mi ->
        case mi of
            JustInteger i -> i
            NoInteger -> def
|]

[s|
    fromMaybeBool :: Bool -> Maybe Bool -> Bool
    fromMaybeBool def = \mb ->
        case mb of
            Just b -> b
            Nothing -> def
|]

[s|
    multclause :: Integer -> Integer
    multclause 2 = 10
    multclause 3 = 20
    multclause 4 = 30   
    multclause 5 = 40
    multclause _ = 50
|]

[s|
    tupleswap :: (a, b) -> (b, a)
    tupleswap (x, y) = (y, x)
|]

[s|
    listswaptop :: [a] -> [a]
    listswaptop (x:xs) = (head xs) : x : (tail xs)

    listdifftop :: [Integer] -> Integer
    listdifftop (x:y:_) = x - y
|]

[s|
    sum2 :: Integer -> Integer -> Integer
    sum2 a b = a + b

    sum3 :: Integer -> Integer -> Integer -> Integer
    sum3 a b c = a + b + c
|]

[s|
    unary2int :: [()] -> Integer
    unary2int [] = 0
    unary2int (_:xs) = 1 + unary2int xs
|]

sth [d|
    class Foo a where
        foo :: a -> Integer
    |]
declvartinst "Foo" ["a"]

[s|
    instance Foo Bool where
        foo _ = 1
    
    instance Foo Integer where
        foo _ = 2
|]

[s|
    foofun :: (Foo a) => a -> Integer
    foofun x = (foo x)*(foo x) + 3*(foo x) + (foo True)
|]

[s|
    data Apple = Apple {
        isgreen :: Bool,
        numseeds :: Integer
    }
|]

[s|
    data Wonderful a = Wonderful {
        thething :: a,
        howgreat :: Integer
    }
|]


sth [d|
        class MultiFoo a b where
            multifoo :: a -> b -> Integer
    |]
declvartinst "MultiFoo" ["a", "b"]

[s|
    instance MultiFoo Bool Bool where
        multifoo _ _ = 1

    instance MultiFoo Integer Integer where
        multifoo _ _ = 2

    instance MultiFoo Bool Integer where
        multifoo _ _ = 3

    instance MultiFoo Integer Bool where
        multifoo _ _ = 4
|]

[s|
    unused :: a -> Integer
    unused x = 42
|]

[s|
    multifoofun :: (MultiFoo a b) => a -> b -> Integer
    multifoofun x y = (multifoo x y) * (multifoo x y) 
                        + 3 * (multifoo x y) + (multifoo True False)
|]

[s|
    -- shadow x = 2*(x+1)
    shadow :: Integer -> Integer
    shadow = \x -> (\x -> 2*x) (x+1)
|]

