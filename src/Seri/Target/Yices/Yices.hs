
module Seri.Target.Yices.Yices (yicesY, compile_decs) where

import Data.Maybe(fromJust)
import qualified Math.SMT.Yices.Syntax as Y

import Seri.Lambda
import Seri.Target.Yices.Compiler
import Seri.Target.Yices.Builtins.Prelude
import Seri.Utils.Ppr

-- Translate a seri expression to a yices expression
yExp :: Compiler -> Exp -> Maybe ([Y.CmdY], Y.ExpY)
yExp _ (IntegerE x) = Just $ ([], Y.LitI x)
yExp _ _ = Nothing

yType :: Compiler -> Type -> Maybe Y.TypY
yType _ _ = Nothing

coreY :: Compiler
coreY = Compiler yExp yType

yicesY :: Compiler
yicesY = compilers [preludeY, coreY]
            
-- compile_dec
--   Assumes the declaration is monomorphic.
compile_dec :: Compiler -> Dec -> [Y.CmdY]
compile_dec c (ValD (Sig n t) e) =
    let Just yt = compile_type c c t
        Just (cmds, ye) = compile_exp c c e
    in cmds ++ [Y.DEFINE (n, yt) (Just ye)]
compile_dec c (DataD n [] cs) =
    let con :: Con -> (String, [(String, Y.TypY)])
        con (Con n ts) = (n, zip [n ++ show i | i <- [0..]]
                                 (map (fromJust . compile_type c c) ts))
    in [Y.DEFTYP n (Just (Y.DATATYPE (map con cs)))]
compile_dec c d
    = error $ "compile_dec: cannot compile to yices: " ++ render (ppr d)

compile_decs :: Compiler -> [Dec] -> [Y.CmdY]
compile_decs c ds = concat $ map (compile_dec c) ds

