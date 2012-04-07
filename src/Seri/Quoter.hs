
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
    boundnames :: [Name],
    freenames :: [Name]
}

initialUserState = UserState [] []

freename :: Name -> State UserState ()
freename nm = modify (\us -> us { freenames = (nm:(freenames us)) })

bindname :: Name -> State UserState ()
bindname nm = modify (\us -> us { boundnames = (nm:(boundnames us)) })

unbindname :: Name -> State UserState ()
unbindname nm = do
    UserState (n:names) fn <- get
    if (n /= nm) 
        then fail $ "unbindname '" ++ show nm ++ "' doesn't match expected '" ++ show n ++ "'"
        else put $ UserState names fn

infixp :: Name -> Exp -> Exp -> Exp
infixp nm a b = apply 'S.infixE [VarE nm, a, b]

-- mkexp :: Exp (a) -> Exp (S.TypedExp a)
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
        else do
            freename nm
            return $ apply 'S.varE_typed [VarE (name_P (nameBase nm)), string nm]

mkexp (ConE nm) = return $ apply 'S.conE_typed [VarE (name_P (nameBase nm)), string nm]

mkexp l@(LitE (IntegerL i)) = return $ apply 'S.integerE [l]

-- Special case for slices.
mkexp (AppE (VarE s) b) | s == mkName "_s"  = return b

mkexp (AppE a b) = do
    a' <- mkexp a
    b' <- mkexp b
    return $ apply 'S.appE [a', b']

mkexp (InfixE (Just a) (VarE op) (Just b)) = do
    a' <- mkexp a
    b' <- mkexp b
    return $ case (nameBase op) of
        "+" -> infixp 'S.addP a' b'
        "-" -> infixp 'S.subP a' b'
        "*" -> infixp 'S.mulP a' b'
        "<" -> infixp 'S.ltP a' b'
        x -> error $ "TODO: infix " ++ show x

mkexp (LamE [VarP nm] a) = do
    bindname nm
    a' <- mkexp a
    unbindname nm
    return $ apply 'S.lamE [string nm, LamE [VarP nm] a']

mkexp (CondE p a b) = do
    p' <- mkexp p
    a' <- mkexp a
    b' <- mkexp b
    return $ apply 'S.ifE [p', a', b']

mkexp (CaseE e matches) = do
    e' <- mkexp e
    ms <- mapM mkmatch matches
    return $ apply 'S.caseE [e', ListE ms]

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
mkpat x = error $ "todo: mkpat " ++ show x

mkvarpnm :: Name -> Name
mkvarpnm nm = mkName ("p_" ++ (nameBase nm))

-- Get the list of variable pattern names in the given pattern.
varps :: Pat -> [Name]
varps (VarP nm) = [nm]
varps (ConP _ ps) = concat (map varps ps)
varps p = error $ "TODO: varps " ++ show p


mkdecls :: [Dec] -> [Dec]
mkdecls [] = []
mkdecls ((SigD nm ty):(ValD (VarP nm') (NormalB e) []):ds) = 
  let (e', UserState _ free) = runState (mkexp e) initialUserState
      typedexp t = (AppT (AppT (ConT ''S.Typed) (ConT ''SIR.Exp)) t)
      ty' = case ty of
                ForallT vns [] t ->
                    let ctx = map (\(PlainTV x) -> ClassP ''S.SeriType [VarT x]) vns
                    in ForallT vns ctx (typedexp t)
                t -> typedexp t
      d = declval' (nameBase nm) ty' e' (map nameBase free)
  in d ++ (mkdecls ds)

s :: QuasiQuoter 
s = QuasiQuoter qexp qpat qtype qdec

qexp :: String -> Q Exp
qexp s = case (parseExp s) of
            Right e -> return (fst $ runState (mkexp e) initialUserState)
            Left err -> fail err

qpat :: String -> Q Pat
qpat = error $ "Seri pattern quasi-quote not supported"

qtype :: String -> Q Type
qtype = error $ "Seri type quasi-quote not supported"

qdec :: String -> Q [Dec]
qdec s = case (parseDecs s) of
            Right decls -> return $ mkdecls decls
            Left err -> fail err

