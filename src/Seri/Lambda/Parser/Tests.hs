
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
        ~=? (run patP "Foo%{Bar} a%{Integer} 42" :: Either String Pat)
    ]

expressions = "Expression" ~: [
    "simple" ~:
        Right (LamE (Sig "x" (ConT "Integer"))
                    (AppE (AppE (VarE (Sig "f" (AppT (AppT (ConT "->") (ConT "Integer")) (ConT "Integer"))) Declared)
                                (VarE (Sig "x" (ConT "Integer")) Bound))
                          (IntegerE 1)))
        ~=? (run expE "\\x.{Integer} -> f%{-> Integer Integer} x.{Integer} 1"
                            :: Either String Exp)
    ]

declarations = "Declaration" ~: [
    "vald simple" ~: 
        Right (ValD (Sig "foo" (ConT "Integer")) (IntegerE 3))
        ~=? (run decD "foo%{Integer} = 3" :: Either String Dec)
    ]

