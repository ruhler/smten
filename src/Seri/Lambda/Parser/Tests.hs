
module Seri.Lambda.Parser.Tests (tests) where

import Test.HUnit

import Seri.Lambda.Parser.Declaration
import Seri.Lambda.Parser.Expression
import Seri.Lambda.Parser.Pattern
import Seri.Lambda.Parser.Type
import Seri.Lambda.Parser.Utils
import Seri.Lambda.IR

tests = "Parser" ~: [
    types,
    patterns,
    expressions,
    declarations
    ]

types = "Type" ~: [
    "simple" ~:
        Right (AppT (AppT (ConT "->") (VarT "a")) (ConT "Integer"))
        ~=? (run typeT "-> a Integer" :: Either String Type),
    "tuple" ~:
        Right (ConT "(,)")
        ~=? (run typeT "(,)" :: Either String Type),
    "forall" ~:
        Right (ForallT ["a", "m"] [ 
                Pred "Foo" [VarT "a"], Pred "Monad" [VarT "m"]]
                (AppT (AppT (ConT "->") (VarT "a")) (AppT (VarT "m") (VarT "a"))))
        ~=? (run typeT "forall a m . (Foo a, Monad m) => -> a (m a)"
                            :: Either String Type)
    ]

patterns = "Pattern" ~: [
    "simple" ~: 
        Right (AppP (AppP (ConP (Sig "Foo" (ConT "Bar")))
                (VarP (Sig "a" (ConT "Integer"))))
                (IntegerP 42)) 
        ~=? (run patP "Foo{Bar} a{Integer} 42" :: Either String Pat),
    "varP" ~:
        Right (VarP (Sig "y" (ConT "Integer")))
        ~=? (run patP "y{Integer}" :: Either String Pat),
    "wildP" ~:
        Right (WildP (ConT "Integer"))
        ~=? (run patP "_{Integer}" :: Either String Pat)
    ]

expressions = "Expression" ~: [
    "simple" ~:
        Right (LamE (Sig "x" (ConT "Integer"))
                    (AppE (AppE (VarE (Sig "f" (AppT (AppT (ConT "->") (ConT "Integer")) (ConT "Integer"))) Declared)
                                (VarE (Sig "x" (ConT "Integer")) Bound))
                          (IntegerE 1)))
        ~=? (run expE "\\x{Integer} -> %f{-> Integer Integer} .x{Integer} 1"
                            :: Either String Exp),
    "paren" ~:
        Right (IntegerE 2)
        ~=? (run expE "(2)" :: Either String Exp),
    "tuple" ~:
        Right (ConE (Sig "(,)" (ConT "(,)")))
        ~=? (run expE "(,){(,)}" :: Either String Exp),
    "lamparen" ~:
        Right (LamE (Sig "_1" (AppT (ConT "[]") (VarT "a"))) (IntegerE 4))
        ~=? (run expE "(\\_1{[] a} -> 4)" :: Either String Exp),
    "operator" ~:
        Right (VarE (Sig "+" (ConT "Integer")) Declared)
        ~=? (run expE "%+{Integer}"
                            :: Either String Exp),
    "integer" ~:
        Right (IntegerE 5) ~=? (run expE "5" :: Either String Exp),
    "match" ~:
        Right (Match (VarP (Sig "y" (ConT "Integer"))) (IntegerE 5))
        ~=? (run matchE "y{Integer} -> 5;" :: Either String Match)
    ]

declarations = "Declaration" ~: [
    "vald simple" ~: 
        Right (ValD (Sig "foo" (ConT "Integer")) (IntegerE 3))
        ~=? (run decD "value foo{Integer} = 3;" :: Either String Dec),
    "data simple" ~:
        Right (DataD "Foo" [] [Con "Bar" [ConT "Integer"], Con "Sludge" [ConT "Bool"]])
        ~=? (run decD "data Foo = Bar Integer | Sludge Bool;"
                        :: Either String Dec),
    "con" ~:
        Right (Con "Bar" [ConT "Integer", ConT "Bool"])
        ~=? (run conD "Bar Integer Bool" :: Either String Con),
    "class simple" ~:
        Right (ClassD "Foo" ["a"] [Sig "foo" (AppT (AppT (ConT "->") (VarT "a")) (ConT "Integer"))])
        ~=? (run decD "class Foo a where  foo{-> a Integer}"
                        :: Either String Dec)
    ]

