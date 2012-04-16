
{-# LANGUAGE TemplateHaskell #-}

module Seri.Quoter (s
    ) where

import Control.Monad.State

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

import qualified Seri.IR as SIR
import qualified Seri.Typed as S

import Seri.THUtils
import Seri.Declarations

data UserState = UserState {
    boundnames :: [Name]
}

bindname :: Name -> State UserState ()
bindname nm = modify (\us -> us { boundnames = (nm:(boundnames us)) })

unbindname :: Name -> State UserState ()
unbindname nm = do
    UserState (n:names) <- get
    if (n /= nm) 
        then fail $ "unbindname '" ++ show nm ++ "' doesn't match expected '" ++ show n ++ "'"
        else put $ UserState names

-- declaredV name
-- Return a reference to a free seri variable declared in the top level
-- environment.
--   name - the seri name.
declaredV :: Name -> State UserState Exp
declaredV nm = return $ apply 'S.dvarE [VarE (declname nm), string nm]

-- declaredC
-- Return a reference to a free seri constructor declared in the top level
-- environment.
--   name - the seri name.
declaredC :: Name -> State UserState Exp
declaredC nm = return $ apply 'S.conE [VarE (declname nm), string nm]

-- mkexp :: Exp (a) -> Exp (S.Typed Exp a)
--   Convert a haskell expression to its corresponding typed seri
--   representation.
--
--   This supports only those haskell expressions which can be represented in
--   the seri IR.
mkexp :: Exp -> State UserState Exp 
mkexp (VarE nm) = do
    bound <- gets boundnames
    if (nm `elem` bound)
        then return $ VarE nm
        else declaredV nm

mkexp (ConE nm) = declaredC nm

mkexp l@(LitE (IntegerL i)) = return $ apply 'S.integerE [l]

-- Special case for slices.
mkexp (AppE (VarE s) b) | s == mkName "_s"  = return b

mkexp (AppE a b) = do
    a' <- mkexp a
    b' <- mkexp b
    return $ apply 'S.appE [a', b']

mkexp (InfixE (Just a) op@(VarE _) (Just b)) = mkexp $ AppE (AppE op a) b
mkexp (InfixE (Just a) op@(ConE _) (Just b)) = mkexp $ AppE (AppE op a) b

mkexp (LamE [VarP nm] a) = do
    bindname nm
    a' <- mkexp a
    unbindname nm
    return $ apply 'S.lamE [string nm, LamE [VarP nm] a']

-- We convert lambda expressions with multiple arguments, such as
--  \a b -> blah
-- To nested lambda expressions of single arguments:
--  \a -> (\b -> blah)
mkexp (LamE (x:xs@(_:_)) a) = mkexp $ LamE [x] (LamE xs a)

mkexp (CondE p a b) = do
    p' <- mkexp p
    a' <- mkexp a
    b' <- mkexp b
    return $ apply 'S.ifE [p', a', b']

mkexp (CaseE e matches) = do
    e' <- mkexp e
    ms <- mapM mkmatch matches
    return $ apply 'S.caseE [e', ListE ms]

mkexp (DoE stmts) = mkexp $ desugar stmts

-- We turn a tuple (a, b, ...) of N elements into
--  (,, ...) a b ...
mkexp (TupE es)
 = let n = length es
       tupn = ConE (mkName $ "(" ++ replicate (n-1) ',' ++ ")")
   in mkexp (foldl AppE tupn es)

-- We turn a list literal [a, b, ...] into its construction:
--  a:b:...:[]
mkexp (ListE es)
 = let desugar :: [Exp] -> Exp
       desugar [] = ConE ''[]
       desugar (x:xs) = InfixE (Just x) (ConE $ mkName ":") (Just (desugar xs))
   in mkexp $ desugar es

mkexp x = error $ "TODO: mkexp " ++ show x

mkmatch :: Match -> State UserState Exp
mkmatch (Match p (NormalB e) [])
  = let lamify :: [Name] -> Exp -> Exp
        lamify [] e = e
        lamify (n:ns) e = lamify ns (apply 'S.lamM [string n, LamE [VarP $ mkvarpnm n, VarP n] e])

        vns = varps p
        p' = mkpat p 
    in do
        mapM_ bindname vns
        e' <- mkexp e
        mapM_ unbindname (reverse vns)
        return $ lamify vns (apply 'S.match [p', e'])

-- Convert a haskell pattern to a Seri pattern.
mkpat :: Pat -> Exp
mkpat (ConP n ps) =
    let mkpat' :: Exp -> [Pat] -> Exp
        mkpat' e [] = e
        mkpat' e (p:ps) = mkpat' (apply 'S.appP [e, mkpat p]) ps
    in mkpat' (apply 'S.conP [string n]) ps
mkpat (VarP n) = VarE $ mkvarpnm n
mkpat (LitP i@(IntegerL _)) = apply 'S.integerP [LitE i]
mkpat WildP = VarE 'S.wildP
mkpat (TupP ps)
 = let n = length ps
       tupn = mkName $ "(" ++ replicate (n-1) ',' ++ ")"
   in mkpat $ ConP tupn ps
mkpat (InfixP a nm b) = mkpat (ConP nm [a, b])
mkpat (ListP []) = mkpat $ ConP (mkName "[]") []
mkpat (ListP (x:xs)) = mkpat $ ConP (mkName ":") [x, ListP xs]
mkpat x = error $ "todo: mkpat " ++ show x

mkvarpnm :: Name -> Name
mkvarpnm nm = mkName ("p_" ++ (nameBase nm))

-- Get the list of variable pattern names in the given pattern.
varps :: Pat -> [Name]
varps (VarP nm) = [nm]
varps (ConP _ ps) = concat (map varps ps)
varps WildP = []
varps (LitP _) = []
varps (TupP ps) = concat (map varps ps)
varps (InfixP a n b) = varps a ++ varps b
varps (ListP ps) = concat (map varps ps)
varps p = error $ "TODO: varps " ++ show p


mkdecls :: [Dec] -> [Dec]
mkdecls [] = []
mkdecls (d@(DataD {}) : ds) = [d] ++ (decltype' d) ++ mkdecls ds
mkdecls ((SigD nm ty):(ValD (VarP _) (NormalB e) []):ds) = 
  let e' = fst $ runState (mkexp e) $ UserState []
      d = declval' nm ty e'
  in d ++ (mkdecls ds)

mkdecls (s@(SigD nm ty):f@(FunD _ clauses):ds) =
  let
      -- Each clause of the form: a b c ... = foo is turned into a case match
      -- of the form: (a, b, c, ...) -> foo.
      --
      -- For this to work, all clauses must have the same number of argument
      -- patterns. We can't yet handle the case where different clauses of the
      -- same function take a different number of arguments. (though it
      -- wouldn't be too difficult to handle that case).
      Clause pats1 _ _ = head clauses
      nargs = length pats1

      mkmatch :: Clause -> Match
      mkmatch (Clause pats _ _) | nargs /= length pats
        = error $ "Not all clauses of the function " ++ show nm ++
                  " have the same number of arguments. " ++
                  "This is not yet supported"
      mkmatch (Clause pats body decls) =
         if nargs == 1
            then Match (head pats) body decls
            else Match (TupP pats) body decls

      -- TODO: hopefully these argnames don't shadow any thing they shouldn't.
      argnames = [mkName $ "_" ++ show i | i <- [1..nargs]]
      casearg = if nargs == 1 then VarE (head argnames) else TupE (map VarE argnames)
      body = LamE (map VarP argnames) $ CaseE casearg (map mkmatch clauses)
      d = ValD (VarP nm) (NormalB body) []
  in mkdecls (s:d:ds)
mkdecls d = error $ "TODO: mkdecls " ++ show d

s :: QuasiQuoter 
s = QuasiQuoter qexp qpat qtype qdec

-- The seri expression quoter returns a haskell value of type
--  Typed Env Exp a
qexp :: String -> Q Exp
qexp s = do
    case (parseExp s) of
            Right e -> do
                let expr = fst $ runState (mkexp e) (UserState [])
                ClassI _ insts  <- reify ''SeriDec
                return $ envize expr insts
            Left err -> fail err

-- envize
-- Given the an expression and a list of SeriDec class instances, return an
-- Envexpression of type [SIR.Dec] with the corresponding seri declarations.
envize :: Exp -> [ClassInstance] -> Exp
envize e insts =
  let tys = map (head . ci_tys) insts
      decs = map (\(ConT n) -> apply 'dec [ConE $ mkName (nameBase n)]) tys
  in apply 'S.enved [e, ListE decs]


qpat :: String -> Q Pat
qpat = error $ "Seri pattern quasi-quote not supported"

qtype :: String -> Q Type
qtype = error $ "Seri type quasi-quote not supported"

qdec :: String -> Q [Dec]
qdec s = case (parseDecs s) of
            Right decls -> return $ map fixUnit (mkdecls decls)
            Left err -> fail err

