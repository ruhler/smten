
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Print SMT syntax to a concrete yices syntax.
module Yices.Concrete (
    YicesVersion(..),
    concrete, pretty
  ) where

import Control.Monad.State.Strict
import Data.Ratio
import Data.List(genericLength)

import Seri.Strict
import Seri.SMT.Syntax

bigsize :: Integer
bigsize = 80

data YicesVersion = Yices1 | Yices2 deriving(Show, Eq)

data SmallSize = Big | Small Integer
    deriving (Eq, Show)

instance Num SmallSize where
    fromInteger i | i < bigsize = Small i
    fromInteger _ = Big

    (+) Big _ = Big
    (+) _ Big = Big
    (+) (Small a) (Small b) = fromInteger (a+b)

    (*) = error $ "(*) SmallSize"
    abs = error $ "abs SmallSize"
    signum = error $ "signum SmallSize"


data CS = CS {
    cs_version :: YicesVersion,
    cs_pretty :: Bool,

    -- We want to clump together into one line anything that fits within some
    -- reasonably small size. This counter increments for each character of
    -- text output, not including indentation. It's used to clump things
    -- together as needed (see the clump function).
    cs_length :: SmallSize,

    cs_indent :: Integer,
    cs_output :: String
}

type ConcreteM = State CS

-- | Convert an abstract syntactic construct to concrete yices syntax.
class Concrete a where
    concreteM :: a -> ConcreteM ()
   

incr :: Integer -> Integer
incr = (+ 1)

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
    let sep = case (cs_pretty cs, str, take 1 (cs_output cs)) of
                (True, _, _) -> "\n"
                (False, "(", _) -> ""
                (False, _, ")") -> ""
                _ -> " "
    put $! cs { cs_output = indented ident $! (str ++ sep ++ cs_output cs),
                cs_length = cs_length cs + genericLength str + 1}

-- | Clump together all the text output by the given ConcreteM if it's a small
-- size and we have pretty printing turned on.
clump :: ConcreteM () -> ConcreteM ()
clump x = do
    nice <- gets cs_pretty
    case nice of
      False -> x
      True -> do
        cs <- get
        put $! cs { cs_length = 0 } 
        x
        len <- gets cs_length
        case len of
            Big -> return ()
            Small _ -> do
                -- It's small, so without pretty it will still look nice.
                -- Re-run the output that way.
                put cs
                v <- gets cs_version
                line $ evalState (x >> gets cs_output) (CS v False 0 0 "")

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
      = clump $ group ("define-type " ++ s) [concreteM td]
    concreteM  (Define s t Nothing)
      = clump $ group ("define " ++ s ++ " ::") [concreteM t]
    concreteM  (Define s t (Just e))
      = clump $ group ("define " ++ s ++ " ::") [concreteM t, concreteM e]
    concreteM  (Assert e)
      = clump $ group "assert" [concreteM e]
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
    concreteM (TupleT ts) = clump $ group "tuple" (map concreteM ts)
    concreteM (ArrowT ts) = clump $ group "->" (map concreteM ts)
    concreteM (BitVectorT i) = line $ "(bitvector " ++ show i ++ ")"
    concreteM IntegerT = line "int"
    concreteM BoolT = line "bool"
    concreteM RealT = line "real"

instance Concrete Expression where
    concreteM (ImmediateE iv) = concreteM iv
    concreteM (ForallE decls e) = clump $ 
      group "forall" [group "" (map concreteM decls), concreteM e]
    concreteM (ExistsE decls e) = clump $ 
      group "exists" [group "" (map concreteM decls), concreteM e]
    concreteM (LetE bindings e) = clump $ 
      group "let" [group "" (map concreteM bindings), concreteM e]
    concreteM (UpdateE f es e) = clump $ 
      group "update" [concreteM f, group "" (map concreteM es), concreteM e]
    concreteM (TupleUpdateE t i x) = clump $ do
      v <- gets cs_version
      let gn = if v == Yices1
                then "update"
                else "tuple-update"
      group gn [concreteM t, line (show i), concreteM x]
    concreteM (FunctionE f args) = clump $ 
      group "" (concreteM f : map concreteM args)

instance Concrete VarDecl where
    concreteM (n, t) = clump $ do
        indent $ concreteM t
        line $ n ++ " ::"

instance Concrete Binding where
    concreteM (n, e) = clump $ group n [concreteM e]

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
pretty v x = evalState (concreteM x >> gets cs_output) (CS v True 0 0 "")

-- | Render abstract yices syntax to a concreteM syntax string meant to be
-- read by a machine.
concrete :: Concrete a => YicesVersion -> a -> String
concrete v x = evalState (concreteM x >> gets cs_output) (CS v False 0 0 "")

