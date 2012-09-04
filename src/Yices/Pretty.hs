
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Print yices syntax prettily
module Yices.Pretty (pretty) where

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
    
instance Concrete Command where
    concrete  (DefineType s Nothing) = do
      line $ "(define-type " ++ s ++ ")"
    concrete  (DefineType s (Just td)) = do
      line ")"
      indent (concrete td)
      line $ "(define-type " ++ s
    concrete  (Define s t Nothing) = do
      line ")"
      indent (concrete t)
      line $ "(define " ++ s ++ " ::"
    concrete  (Define s t (Just e)) = do
      line ")"
      indent (concrete e)
      indent (concrete t)
      line $ "(define " ++ s ++ " ::"
    concrete  (Assert e) = do
      line ")"
      indent (concrete e)
      line "(assert"
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
    concrete (TupleT ts) = do
      line ")"
      indent (mapM concrete (reverse ts))
      line "(tuple"
    concrete (ArrowT ts) = do
      line ")"
      indent (mapM concrete (reverse ts))
      line "(->"
    concrete (BitVectorT i) = line $ "(bitvector " ++ show i ++ ")"
    concrete IntegerT = line "int"
    concrete BoolT = line "bool"
    concrete RealT = line "real"

instance Concrete Expression where
    concrete (ImmediateE iv) = concrete iv
    concrete (ForallE decls e) = do
      line ")"
      indent (concrete e)
      indent $ do
        line ")"
        indent (mapM concrete (reverse decls))
        line "("
      line "(forall"
    concrete (ExistsE decls e) = do
      line ")"
      indent (concrete e)
      indent $ do
        line ")"
        indent (mapM concrete (reverse decls))
        line "("
      line "(exists"
    concrete (LetE bindings e) = do
      line ")"
      indent (concrete e)
      indent $ do
        line ")"
        indent (mapM concrete (reverse bindings))
        line "("
      line "(let"
    concrete (UpdateE f es e) = do
      line ")"
      indent (concrete e)
      indent $ do
        line ")"
        indent (mapM concrete (reverse es))
        line "("
      indent (concrete f)
      line "(update"
    concrete (TupleUpdateE t i x) = do
      line ")"
      indent (concrete x)
      indent (line (show i))
      indent (concrete t)
      v <- gets cs_version
      if v == Yices1
        then line "(update"
        else line "(tuple-update"
    concrete (FunctionE f args) = do
          line ")"
          indent (mapM concrete (reverse args))
          indent (concrete f)
          line "("

instance Concrete VarDecl where
    concrete (n, t) = do
        indent $ concrete t
        line $ n ++ " ::"

instance Concrete Binding where
    concrete (n, e) = do
      line ")"
      indent $ concrete e
      line $ "(" ++ n

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

