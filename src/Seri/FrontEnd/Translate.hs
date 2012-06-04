
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.FrontEnd.Translate (Translate(..)) where

import Control.Monad.State
import Data.Maybe

import Language.Haskell.TH

import qualified Seri.FrontEnd.Typed as S

import Seri.Utils.TH
import Seri.FrontEnd.Canonical
import Seri.FrontEnd.Declarations.Names
import Seri.FrontEnd.Declarations.SeriDec
import Seri.FrontEnd.Declarations.Library
import Seri.FrontEnd.Slice

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
declaredV nm = return $ apply 'S.dvarE [VarE (valuename nm), VarE (instidname nm), string nm]

-- declaredC
-- Return a reference to a free seri constructor declared in the top level
-- environment.
--   name - the seri name.
declaredC :: Name -> State UserState Exp
declaredC nm = return $ apply 'S.conE [VarE (valuename nm), string nm]

-- mkexp :: Exp (a) -> Exp (S.Typed Exp a)
--   Convert a haskell expression to its corresponding typed seri
--   representation.
--
--   This supports only those haskell expressions which can be represented in
--   the seri IR.
mkexp :: Exp -> State UserState Exp 

-- Special case for slices.
mkexp e | sliceof e /= Nothing = return $ fromJust (sliceof e)

mkexp (VarE nm) = do
    bound <- gets boundnames
    if (nm `elem` bound)
        then return $ VarE nm
        else declaredV nm

mkexp (ConE nm) = declaredC nm

mkexp l@(LitE (IntegerL i)) = return $ apply 'S.integerE [l]


mkexp (AppE a b) = do
    a' <- mkexp a
    b' <- mkexp b
    return $ apply 'S.appE [a', b']

mkexp (LamE [VarP nm] a) = do
    bindname nm
    a' <- mkexp a
    unbindname nm
    return $ apply 'S.lamE [string nm, LamE [VarP nm] a']

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
    in mkpat' (apply 'S.conP [VarE (valuename n), string n]) ps
mkpat (VarP n) = VarE $ mkvarpnm n
mkpat (LitP i@(IntegerL _)) = apply 'S.integerP [LitE i]
mkpat WildP = VarE 'S.wildP
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

mkdecls ((InstanceD c t ids):ds) =
  let mkid :: Dec -> Dec
      mkid (ValD p (NormalB b) []) =
        let b' = fst $ runState (mkexp b) $ UserState []
        in ValD p (NormalB b') []
      
      ids' = map mkid ids
  in declinst' (InstanceD c t ids') ++ mkdecls ds
mkdecls (d@(ClassD {}):ds) = declclass' d ++ mkdecls ds

mkdecls d = error $ "TODO: mkdecls " ++ show d

class Translate a where
    translate :: a -> Q a

instance Translate Exp where
    translate e = withdecs (fst $ runState (mkexp . canonical $ e) (UserState []))

instance Translate [Dec] where
    translate decls = return $ map fixUnit (mkdecls . canonical $ decls)

