
{-# LANGUAGE QuasiQuotes #-}

import Seri
import SeriPrint
import Elaborate
import SeriHaskell
import CPrint
import qualified C
import SeriC
import SeriTypeCheck
import SeriParser
import SeriQuoter

-- foo: (\x = x*x + 3*x + 2) 5
foo :: Exp
foo =
 let x = VarE IntegerT "x"
     body = AddE (AddE (MulE x x) (MulE (IntegerE 3) x)) (IntegerE 2)
     lam = LamE (ArrowT IntegerT IntegerT) "x" body
 in AppE (ArrowT IntegerT IntegerT) lam (IntegerE 5)

fooc :: ([C.Dec], C.Exp)
fooc = c foo

qfoo :: Exp
qfoo = [s|(\x -> x*x+3*x+2) 5|]

main :: IO ()
main = do
    tfoo <- typecheck foo
    putStrLn $ show tfoo

    putStrLn $ "Foo: " ++ show foo
    putStrLn $ "Foo Elaborated: " ++ show (elaborate foo)
    putStrLn $ "Foo Pretty: " ++ show (ppr foo)
    putStrLn $ "Haskell Foo: " ++ show (ppr (haskell foo))
    let (cd, ce) = fooc
    putStrLn $ "C Foo: " ++ show (ppr cd, ppr ce)

    parsed <- seriparse "(\\x -> x*x+3*x+2) 5"
    putStrLn $ "Parsed: " ++ show parsed
    putStrLn $ "Parsed Elaborated: " ++ show (elaborate parsed)

    putStrLn $ "Q Foo: " ++ show qfoo
    putStrLn $ "Q Foo Elaborated: " ++ show (elaborate qfoo)

