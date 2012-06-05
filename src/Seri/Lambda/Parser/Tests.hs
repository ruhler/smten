
module Seri.Lambda.Parser.Tests (tests) where

import Test.HUnit

import Seri.Lambda.Parser.Type
import Seri.Lambda.Parser.Utils
import Seri.Lambda.IR

tests = "Parser" ~: [
    "simple type" ~:
        Right (AppT (AppT (ConT "->") (VarT "a")) (ConT "Integer"))
        ~=? (run typeT "-> a Integer" :: Either String Type),
    "forall type" ~:
        Right (ForallT ["a", "m"] [ 
                Pred "Foo" [VarT "a"], Pred "Monad" [VarT "m"]]
                (AppT (AppT (ConT "->") (VarT "a")) (AppT (VarT "m") (VarT "a"))))
        ~=? (run typeT "forall a m . (Foo a, Monad m) => -> a (m a)"
                            :: Either String Type)
    ]

