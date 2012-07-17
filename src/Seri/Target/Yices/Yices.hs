
module Seri.Target.Yices.Yices (
    Compilation(), compilation, CompilationM, runCompilation,
    yicesN, yicesT, yicesE,
    ) where

import qualified Math.SMT.Yices.Syntax as Y

import Data.List ((\\))
import Data.Char (ord)
import Data.Generics
import Control.Monad.State

import Seri.Failable
import Seri.Lambda
import Seri.Target.Monomorphic.Monomorphic
import Seri.Target.Elaborate
import Seri.Target.Inline

-- | A yices compilation object.
data Compilation = Compilation {
    ys_idepth :: Integer,  -- ^ Depth of inlining to perform
    ys_poly :: Env,        -- ^ The polymorphic seri environment
    ys_decl :: Env,        -- ^ Already declared (monomorphic) declarations
    ys_cmds :: [Y.CmdY],   -- ^ Declarations needed for what was compiled
    ys_errid :: Integer,   -- ^ unique id to use for next free error variable
    ys_caseid :: Integer   -- ^ unique id to use for next case arg variable
}

-- | Monad for performing additional yices compilation.
type CompilationM = StateT Compilation Failable

-- | Create a new yices compilation object.
compilation :: Integer         -- ^ inline depth
               -> Env          -- ^ polymorphic seri environment
               -> Compilation
compilation idepth poly = Compilation {
    ys_idepth = idepth,
    ys_poly = poly,
    ys_decl = [],
    ys_cmds = [],
    ys_errid = 1,
    ys_caseid = 1 
}

-- | Convert a seri name to a yices name.
yicesN :: String -> String
yicesN = yicesname

-- | Compile a seri type to a yices type
-- Returns a list of yices commands needed to use the compiled type.
yicesT :: Type -> CompilationM ([Y.CmdY], Y.TypY)
yicesT t = do
   mt <- compileNeeded t 
   yt <- lift $ yType mt
   cmds <- gets ys_cmds
   modify $ \ys -> ys { ys_cmds = [] }
   return (cmds, yt)

-- | Compile a seri expression to a yices expression.
-- Returns a list of yices commands needed to use the compiled expressions.
yicesE :: Exp -> CompilationM ([Y.CmdY], Y.ExpY)
yicesE e = do
    idepth <- gets ys_idepth
    poly <- gets ys_poly
    let ie = inline idepth poly e
    se <- elaborate simplifyR poly ie
    me <- compileNeeded (errorize se)
    ye <- yExp me
    cmds <- gets ys_cmds
    modify $ \ys -> ys { ys_cmds = [] }
    return (cmds, ye)

-- | Run a compilation.
runCompilation :: CompilationM a -> Compilation -> Failable (a, Compilation)
runCompilation = runStateT

yfail :: String -> CompilationM a
yfail = lift . fail

addcmds :: [Y.CmdY] -> CompilationM ()
addcmds cmds = modify $ \ys -> ys { ys_cmds = ys_cmds ys ++ cmds }

-- Given some object, compile everything in the environment needed for this
-- object, and return the monomorphic object.
compileNeeded :: (Monomorphic a, Ppr a) => a -> CompilationM a
compileNeeded x = do
    poly <- gets ys_poly
    let (mdecs, mx) = monomorphic poly x
    let (sorted, r) = sort mdecs
    if null r
        then return ()  
        else yfail $ "yices recursive declarations not supported: "
                ++ pretty r ++ ",\n needed for " ++ pretty x
    decl <- gets ys_decl
    let ndecl = sorted \\ decl
    mapM_ yDec ndecl
    modify $ \ys -> ys { ys_decl = decl ++ ndecl }
    return mx
    
    
    

-- Given the type a free error variable, return the yices name of a newly
-- defined one.
yfreeerr :: Type -> CompilationM String
yfreeerr t = do
    yt <- lift $ yType t
    id <- gets ys_errid
    let nm = yicesname ("err~" ++ show id)
    modify $ \ys -> ys { ys_errid = id+1 }
    addcmds [Y.DEFINE (nm, yt) Nothing]
    return nm

yfreecase :: CompilationM String
yfreecase = do
    id <- gets ys_caseid
    modify $ \ys -> ys { ys_caseid = id+1 }
    return $ yicesname ("c~" ++ show id)

-- Translate a seri expression to a yices expression
yExp :: Exp -> CompilationM Y.ExpY
yExp (LitE (IntegerL x)) = return $ Y.LitI x
yExp (LitE (CharL c)) = return $ Y.LitI (fromIntegral $ ord c)
yExp e@(CaseE _ []) = yfail $ "empty case statement: " ++ pretty e
yExp (CaseE e ms) =
  let -- depat p e
      --    outputs: (predicate, bindings)
      --   predicate - predicates indicating if the 
      --                pattern p matches expression e
      --   bindings - a list of bindings made when p matches e.
      depat :: Pat -> Y.ExpY -> ([Y.ExpY], [((String, Maybe Y.TypY), Y.ExpY)])
      depat (ConP _ n ps) e =
        let (preds, binds) = unzip [depat p (Y.APP (Y.VarE (yicesname n ++ show i)) [e])
                                    | (p, i) <- zip ps [0..]]
            mypred = Y.APP (Y.VarE (yicesname n ++ "?")) [e]
        in (mypred:(concat preds), concat binds)
      depat (VarP (Sig n t)) e = ([], [((n, attemptM $ yType t), e)])
      depat (IntegerP i) e = ([Y.LitI i Y.:= e], [])
      depat (WildP _) _ = ([], [])

      ylet :: [((String, Maybe Y.TypY), Y.ExpY)] -> Y.ExpY -> Y.ExpY
      ylet [] e = e
      ylet bindings e = Y.LET bindings e

      -- take the AND of a list of predicates in a reasonable way.
      yand :: [Y.ExpY] -> Y.ExpY
      yand [] = Y.VarE "true"
      yand [x] = x
      yand xs = Y.AND xs

      -- dematch e ms
      --    e - the expression being cased on
      --    ms - the remaining matches in the case statement.
      --  outputs - the yices expression implementing the matches.
      dematch :: Y.ExpY -> [Match] -> CompilationM Y.ExpY
      dematch ye [] = do
          errnm <- yfreeerr (arrowsT [typeof e, typeof (head ms)])
          return $ Y.APP (Y.VarE errnm) [ye]
      dematch e ((Match p b):ms) = do
          bms <- dematch e ms
          b' <- yExp b
          let (preds, bindings) = depat p e
          let pred = yand preds
          return $ Y.IF pred (ylet bindings b') bms
  in do
      e' <- yExp e
      cnm <- yfreecase
      body <- dematch (Y.VarE cnm) ms
      return $ ylet [((cnm, Nothing), e')] body
yExp (VarE (Sig "~error" t)) = do
    errnm <- yfreeerr t
    return $ Y.VarE errnm
yExp (AppE a b) = do
    a' <- yExp a
    b' <- yExp b
    return $ Y.APP a' [b']
yExp (LamE (Sig n t) e) = do
    e' <- yExp e
    t' <- lift $ yType t
    return $ Y.LAMBDA [(n, t')] e'
yExp (ConE (Sig n _)) = return $ Y.VarE (yicescon n)
yExp (VarE (Sig n _)) = return $ Y.VarE (yicesname n)

-- Yices data constructors don't support partial application, so we wrap them in
-- functions given by the following name.
yicescon :: Name -> Name
yicescon n = yicesname $ "C" ++ n

yType :: Type -> Failable Y.TypY
yType (ConT n) = return $ Y.VarT (yicesname n)
yType (AppT (AppT (ConT "->") a) b) = do
    a' <- yType a
    b' <- yType b
    return $ Y.ARR [a', b']
yType t = fail $ "Cannot compile to yices: " ++ pretty t

-- yDec
--   Assumes the declaration is monomorphic.
--   Adds itself to the cmds list.
yDec :: Dec -> CompilationM ()
yDec (ValD (TopSig n [] t) e) = do
    yt <- lift $ yType t
    ye <- yExp e
    addcmds [Y.DEFINE (yicesname n, yt) (Just ye)]
yDec (DataD "Integer" _ _) =
    let deftype = Y.DEFTYP "Integer" (Just (Y.VarT "int"))
    in addcmds [deftype]
yDec (DataD "Char" _ _) = addcmds [Y.DEFTYP "Char" (Just (Y.VarT "int"))]
yDec (DataD n [] cs) =
    let con :: Con -> CompilationM (String, [(String, Y.TypY)])
        con (Con n ts) = do 
            ts' <- lift $ mapM yType ts
            return (yicesname n, zip [yicesname n ++ show i | i <- [0..]] ts')

        -- Wrap each constructor in a function which supports partial
        -- application.
        mkcons :: Con -> CompilationM Y.CmdY
        mkcons (Con cn ts) = do
            yts <- lift $ mapM yType ts
            let ft a b = Y.ARR [a, b]
            let yt = foldr ft (Y.VarT (yicesname n)) yts
            let fe (n, t) e = Y.LAMBDA [(n, t)] e
            let names = [[c] | c <- take (length ts) "abcdefghijklmnop"]
            let body =  if null ts
                          then Y.VarE (yicesname cn)
                          else Y.APP (Y.VarE (yicesname cn)) (map Y.VarE names)
            let ye = foldr fe body (zip names yts)
            return $ Y.DEFINE (yicescon cn, yt) (Just ye)
    in do
        cs' <- mapM con cs
        let deftype = Y.DEFTYP (yicesname n) (Just (Y.DATATYPE cs'))
        defcons <- mapM mkcons cs
        addcmds $ deftype : defcons

-- Integer Primitives
yDec (PrimD (TopSig "__prim_add_Integer" _ _))
 = addcmds [defiop "__prim_add_Integer" "+"]
yDec (PrimD (TopSig "__prim_sub_Integer" _ _))
 = addcmds [defiop "__prim_sub_Integer" "-"]
yDec (PrimD (TopSig "<" _ _)) = addcmds [defbop "<" "<"]
yDec (PrimD (TopSig ">" _ _)) = addcmds [defbop ">" ">"]
yDec (PrimD (TopSig "__prim_eq_Integer" _ _))
 = addcmds [defbop "__prim_eq_Integer" "="]
yDec (PrimD (TopSig "error" _ _)) = return ()

yDec d = yfail $ "Cannot compile to yices: " ++ pretty d

-- Replace all occurences of the "error" primitive with our own.
-- This gets rid of the string argument, which we can't compile well to
-- yices1, because it requires lists, which we don't support.
errorize :: (Data a, Ppr a) => a -> a
errorize =
  let f :: Exp -> Exp
      f e@(AppE (VarE (Sig "error" _)) _) = VarE (Sig "~error" (typeof e))
      f x = x

  in everywhere (mkT f)

-- defiop name type op
--   Define a primitive binary integer operation.
--   name - the name of the primitive
--   op - the integer operation.
defiop :: String -> String -> Y.CmdY
defiop name op =
    Y.DEFINE (yicesname name, Y.VarT "(-> Integer (-> Integer Integer))")
        (Just (Y.VarE $
            "(lambda (a::Integer) (lambda (b::Integer) (" ++ op ++ " a b)))"))

-- defbop name type op
--   Define a primitive binary integer predicate.
--   name - the name of the primitive
--   op - the predicate operator.
defbop :: String -> String -> Y.CmdY
defbop name op =
    Y.DEFINE (yicesname name, Y.VarT "(-> Integer (-> Integer Bool))")
        (Just (Y.VarE $ unlines [
                "(lambda (a::Integer) (lambda (b::Integer)",
                " (if (" ++ op ++ " a b) True False)))"]))


-- Given a seri identifer, turn it into a valid yices identifier.
-- TODO: hopefully our choice of names won't clash with the users choices...
--
-- I don't have documentation for what yices allows in names, but it appears
-- symbols aren't allowed. So this just replaces each symbol with an ascii
-- approximation.
yicesname :: String -> String
yicesname [] = []
-- TODO: renaming of 'not' should be part of builtins, it should not go here.
yicesname "not" = "_not"
yicesname ('!':cs) = "__bang" ++ yicesname cs
yicesname ('#':cs) = "__hash" ++ yicesname cs
yicesname ('%':cs) = "__percent" ++ yicesname cs
yicesname ('&':cs) = "__amp" ++ yicesname cs
yicesname ('*':cs) = "__star" ++ yicesname cs
yicesname ('+':cs) = "__plus" ++ yicesname cs
yicesname ('.':cs) = "__dot" ++ yicesname cs
yicesname ('/':cs) = "__slash" ++ yicesname cs
yicesname ('<':cs) = "__lt" ++ yicesname cs
yicesname ('=':cs) = "__eq" ++ yicesname cs
yicesname ('>':cs) = "__gt" ++ yicesname cs
yicesname ('?':cs) = "__ques" ++ yicesname cs
yicesname ('@':cs) = "__at" ++ yicesname cs
yicesname ('\\':cs) = "__bslash" ++ yicesname cs
yicesname ('^':cs) = "__hat" ++ yicesname cs
yicesname ('|':cs) = "__bar" ++ yicesname cs
yicesname ('-':cs) = "__dash" ++ yicesname cs
yicesname ('(':cs) = "__oparen" ++ yicesname cs
yicesname (')':cs) = "__cparen" ++ yicesname cs
yicesname (',':cs) = "__comma" ++ yicesname cs
yicesname (c:cs) = c : yicesname cs

