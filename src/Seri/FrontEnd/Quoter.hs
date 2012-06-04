
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.FrontEnd.Quoter (s, STH(..)) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

import Seri.FrontEnd.Translate

s :: QuasiQuoter 
s = QuasiQuoter qexp qpat qtype qdec

-- The seri expression quoter returns a haskell value of type
--  Typed Env Exp a
qexp :: String -> Q Exp
qexp s = do
    case (parseExp s) of
            Right e -> translate e
            Left err -> fail err

qpat :: String -> Q Pat
qpat = error $ "Seri pattern quasi-quote not supported"

qtype :: String -> Q Type
qtype = error $ "Seri type quasi-quote not supported"

qdec :: String -> Q [Dec]
qdec s = case (parseDecs s) of
            Right decls -> translate decls
            Left err -> fail err

-- Support for template haskell quoting.
--  The downside of using template haskell quotes for the front end is you
--  have to typecheck before quoting, which can be silly.
--
--  The advantage is, class declarations are supported.
class STH a where
    sth :: Q a -> Q a

instance STH Exp where
    sth qe = qe >>= translate

instance STH [Dec] where
    sth qd = qd >>= translate

