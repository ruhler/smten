
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Print yices syntax concretely
module Yices.Concrete (concrete, pretty) where

import Control.Monad.State.Strict
import Data.Ratio

import Yices.Syntax

data CS = CS {
    cs_version :: YicesVersion,
    cs_pretty :: Bool,
    cs_indent :: Integer,
    cs_output :: String
}

type ConcreteM = State CS

-- | Convert an abstract syntactic construct to concrete yices syntax.
class Concrete a where
    concreteM :: a -> ConcreteM ()

incr :: Integer -> Integer
incr = (+ 1)

modifyS :: (CS -> CS) -> ConcreteM ()
modifyS f = do
    cs <- get
    put $! f cs

indent :: ConcreteM a -> ConcreteM a
indent x = do
    nice <- gets cs_pretty
    if nice
      then do
        ident <- gets cs_indent
        modifyS $ \cs -> cs {cs_indent = incr $! ident }
        r <- x
        modify $ \cs -> cs { cs_indent = ident }
        return r
      else x

indented :: Integer -> String -> String
indented 0 s = s
indented n s = ' ' : indented (n-1) s

line :: String -> ConcreteM ()
line str = do
    cs <- get
    let ident = cs_indent cs
    let sep = if cs_pretty cs then '\n' else ' '
    put $! cs { cs_output = indented ident $! (str ++ (sep : (cs_output cs))) }

-- | Given the name of an element e and a list of components [a, b, ...],
-- generate a the grouping: (e a b ...)
group :: String -> [ConcreteM ()] -> ConcreteM ()
group str elems = do
    line ")"
    indent $ sequence (reverse elems)
    line $ "(" ++ str
    
instance Concrete Command where
    concreteM  (DefineType s Nothing)
      = line $ "(define-type " ++ s ++ ")"
    concreteM  (DefineType s (Just td))
      = group ("define-type " ++ s) [concreteM td]
    concreteM  (Define s t Nothing)
      = group ("define " ++ s ++ " ::") [concreteM t]
    concreteM  (Define s t (Just e))
      = group ("define " ++ s ++ " ::") [concreteM t, concreteM e]
    concreteM  (Assert e)
      = group "assert" [concreteM e]
    concreteM Check = line "(check)"
    concreteM Push = line "(push)"
    concreteM Pop = line "(pop)"

instance Concrete [Command] where
    concreteM cmds = mapM_ concreteM (reverse cmds)

instance Concrete Typedef where
    concreteM (ScalarTD ss) = do
      line $ "(scalar " ++ concat (map ((:) ' ') ss) ++ ")"
    concreteM (NormalTD t) = concreteM t

instance Concrete Type where
    concreteM (VarT s) = line s
    concreteM (TupleT ts) = group "tuple" (map concreteM ts)
    concreteM (ArrowT ts) = group "->" (map concreteM ts)
    concreteM (BitVectorT i) = line $ "(bitvector " ++ show i ++ ")"
    concreteM IntegerT = line "int"
    concreteM BoolT = line "bool"
    concreteM RealT = line "real"

instance Concrete Expression where
    concreteM (ImmediateE iv) = concreteM iv
    concreteM (ForallE decls e) = do
      group "forall" [group "" (map concreteM decls), concreteM e]
    concreteM (ExistsE decls e) = do
      group "exists" [group "" (map concreteM decls), concreteM e]
    concreteM (LetE bindings e) = do
      group "let" [group "" (map concreteM bindings), concreteM e]
    concreteM (UpdateE f es e) = do
      group "update" [concreteM f, group "" (map concreteM es), concreteM e]
    concreteM (TupleUpdateE t i x) = do
      v <- gets cs_version
      let gn = if v == Yices1
                then "update"
                else "tuple-update"
      group gn [concreteM t, line (show i), concreteM x]
    concreteM (FunctionE f args) = do
      group "" (concreteM f : map concreteM args)

instance Concrete VarDecl where
    concreteM (n, t) = do
        indent $ concreteM t
        line $ n ++ " ::"

instance Concrete Binding where
    concreteM (n, e) = group n [concreteM e]

instance Concrete ImmediateValue where
    concreteM TrueV = line "true"
    concreteM FalseV = line "false"
    concreteM (VarV s) = line s
    concreteM (RationalV r) = do
      line $ show (numerator r) ++
                if denominator r == 1
                    then ""
                    else "/" ++ show (denominator r)

-- | Render abstract yices syntax to a concreteM syntax string meant to be
-- read by a human.
pretty :: Concrete a => YicesVersion -> a -> String
pretty v x = evalState (concreteM x >> gets cs_output) (CS v True 0 "")

-- | Render abstract yices syntax to a concreteM syntax string meant to be
-- read by a machine.
concrete :: Concrete a => YicesVersion -> a -> String
concrete v x = evalState (concreteM x >> gets cs_output) (CS v False 0 "")

