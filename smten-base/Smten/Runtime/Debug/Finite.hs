
module Smten.Runtime.Debug.Finite (
    debug_BoolFF, traceS_BoolFF,
    debug_IntegerFF, traceS_IntegerFF,
    debug_BitFF, traceS_BitFF,
    ) where

import Smten.Runtime.Debug
import Smten.Runtime.Formula.Finite
import Smten.Runtime.FreeID

debug_BoolFF :: BoolFF -> Debug
debug_BoolFF = dbgb

traceS_BoolFF :: BoolFF -> String
traceS_BoolFF = dbgShow . debug_BoolFF

debug_IntegerFF :: IntegerFF -> Debug
debug_IntegerFF = dbgi

traceS_IntegerFF :: IntegerFF -> String
traceS_IntegerFF = dbgShow . debug_IntegerFF

debug_BitFF :: BitFF -> Debug
debug_BitFF = dbgv

traceS_BitFF :: BitFF -> String
traceS_BitFF = dbgShow . debug_BitFF


-- mark a debug object for sharing.
sh :: Debug -> Debug
sh x = dbgShare id x

shb :: BoolFF -> Debug
shb x = sh (dbgb x)

shi :: IntegerFF -> Debug
shi x = sh (dbgi x)

shv :: BitFF -> Debug
shv x = sh (dbgv x)

op :: String -> Debug -> Debug -> Debug
op o a b = dbgOp o (sh a) (sh b)

opb :: String -> BoolFF -> BoolFF -> Debug
opb o a b = op o (dbgb a) (dbgb b)

opi :: String -> IntegerFF -> IntegerFF -> Debug
opi o a b = op o (dbgi a) (dbgi b)

opv :: String -> BitFF -> BitFF -> Debug
opv o a b = op o (dbgv a) (dbgv b)


dbgb :: BoolFF -> Debug
dbgb x =
  case x of 
    TrueFF -> dbgLit True
    FalseFF -> dbgLit False
    IteFF p a b _ -> dbgCase "True" (shb p) (shb a) (shb b)
    AndFF a b _ -> opb "&&" a b
    OrFF a b _ -> opb "||" a b
    NotFF x _ -> dbgApp (dbgText "!") (shb x)
    VarFF n _ -> dbgVar (freenm n)
    Eq_IntegerFF a b _ -> opi "==" a b
    Leq_IntegerFF a b _ -> opi "<=" a b
    Eq_BitFF a b _ -> opv "==" a b
    Leq_BitFF a b _ -> opv "<=" a b
    Unreachable_BoolFF -> dbgText "UR"

dbgi :: IntegerFF -> Debug
dbgi x =
  case x of
    IntegerFF i -> dbgLit i
    Add_IntegerFF a b _ -> opi "+" a b
    Sub_IntegerFF a b _ -> opi "-" a b
    Ite_IntegerFF p a b _ -> dbgCase "True" (shb p) (shi a) (shi b)
    Var_IntegerFF n _ -> dbgVar (freenm n)
    Unreachable_IntegerFF -> dbgText "UR"

dbgv :: BitFF -> Debug
dbgv x =
  case x of
    BitFF v -> dbgLit v
    Add_BitFF a b _ -> opv "+" a b
    Sub_BitFF a b _ -> opv "-" a b
    Mul_BitFF a b _ -> opv "*" a b
    SDiv_BitFF a b _ -> opv "`sdiv`" a b
    SRem_BitFF a b _ -> opv "`srem`" a b
    SMod_BitFF a b _ -> opv "`smod`" a b
    UDiv_BitFF a b _ -> opv "`udiv`" a b
    URem_BitFF a b _ -> opv "`urem`" a b
    Or_BitFF a b _ -> opv "|" a b
    And_BitFF a b _ -> opv "&" a b
    Shl_BitFF _ a b _ -> opv "<<" a b
    Lshr_BitFF _ a b _ -> opv ">>" a b
    Concat_BitFF a b _ -> opv "++" a b
    Not_BitFF a _ -> dbgApp (dbgText "~") (shv x)
    SignExtend_BitFF fr to x _ -> dbgText "?SignExtend"
    Extract_BitFF hi lo x _ -> dbgApp (shv x) (dbgText $ "[" ++ show hi ++ ":" ++ show lo ++ "]")
    Ite_BitFF p a b _ -> dbgCase "True" (shb p) (shv a) (shv b)
    Var_BitFF _ n _ -> dbgVar (freenm n)
    Unreachable_BitFF -> dbgText "UR"

