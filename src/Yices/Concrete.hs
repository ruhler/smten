
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Print yices syntax concretely
module Yices.Concrete (pretty) where

import Control.Monad.State.Strict
import Data.Ratio

import Yices.Syntax

data CS = CS {
    cs_version :: YicesVersion,
    cs_indent :: Integer,
    cs_output :: String
}

type ConcreteM = State CS

-- | Convert an abstract syntactic construct to concrete yices syntax.
class Concrete a where
    concrete :: a -> ConcreteM ()

incr :: Integer -> Integer
incr = (+ 1)

modifyS :: (CS -> CS) -> ConcreteM ()
modifyS f = do
    cs <- get
    put $! f cs

indent :: ConcreteM a -> ConcreteM a
indent x = do
    ident <- gets cs_indent
    modifyS $ \cs -> cs {cs_indent = incr $! ident }
    r <- x
    modify $ \cs -> cs { cs_indent = ident }
    return r

indented :: Integer -> String -> String
indented 0 s = s
indented n s = ' ' : indented (n-1) s

line :: String -> ConcreteM ()
line str = do
    cs <- get
    let ident = cs_indent cs
    put $! cs { cs_output = indented ident $! (str ++ ('\n' : (cs_output cs))) }

-- | Given the name of an element e and a list of components [a, b, ...],
-- generate a the grouping: (e a b ...)
group :: String -> [ConcreteM ()] -> ConcreteM ()
group str elems = do
    line ")"
    indent $ sequence (reverse elems)
    line $ "(" ++ str
    
instance Concrete Command where
    concrete  (DefineType s Nothing)
      = line $ "(define-type " ++ s ++ ")"
    concrete  (DefineType s (Just td))
      = group ("define-type " ++ s) [concrete td]
    concrete  (Define s t Nothing)
      = group ("define " ++ s ++ " ::") [concrete t]
    concrete  (Define s t (Just e))
      = group ("define " ++ s ++ " ::") [concrete t, concrete e]
    concrete  (Assert e)
      = group "assert" [concrete e]
    concrete Check = line "(check)"
    concrete Push = line "(push)"
    concrete Pop = line "(pop)"

instance Concrete [Command] where
    concrete cmds = mapM_ concrete (reverse cmds)

instance Concrete Typedef where
    concrete (ScalarTD ss) = do
      line $ "(scalar " ++ concat (map ((:) ' ') ss) ++ ")"
    concrete (NormalTD t) = concrete t

instance Concrete Type where
    concrete (VarT s) = line s
    concrete (TupleT ts) = group "tuple" (map concrete ts)
    concrete (ArrowT ts) = group "->" (map concrete ts)
    concrete (BitVectorT i) = line $ "(bitvector " ++ show i ++ ")"
    concrete IntegerT = line "int"
    concrete BoolT = line "bool"
    concrete RealT = line "real"

instance Concrete Expression where
    concrete (ImmediateE iv) = concrete iv
    concrete (ForallE decls e) = do
      group "forall" [group "" (map concrete decls), concrete e]
    concrete (ExistsE decls e) = do
      group "exists" [group "" (map concrete decls), concrete e]
    concrete (LetE bindings e) = do
      group "let" [group "" (map concrete bindings), concrete e]
    concrete (UpdateE f es e) = do
      group "update" [concrete f, group "" (map concrete es), concrete e]
    concrete (TupleUpdateE t i x) = do
      v <- gets cs_version
      let gn = if v == Yices1
                then "update"
                else "tuple-update"
      group gn [concrete t, line (show i), concrete x]
    concrete (FunctionE f args) = do
      group "" (concrete f : map concrete args)

instance Concrete VarDecl where
    concrete (n, t) = do
        indent $ concrete t
        line $ n ++ " ::"

instance Concrete Binding where
    concrete (n, e) = group n [concrete e]

instance Concrete ImmediateValue where
    concrete TrueV = line "true"
    concrete FalseV = line "false"
    concrete (VarV s) = line s
    concrete (RationalV r) = do
      line $ show (numerator r) ++
                if denominator r == 1
                    then ""
                    else "/" ++ show (denominator r)

-- | Render abstract yices syntax to a concrete syntax string.
pretty :: Concrete a => YicesVersion -> a -> String
pretty v x = evalState (concrete x >> gets cs_output) (CS v 0 "")

