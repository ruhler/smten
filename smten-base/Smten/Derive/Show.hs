
{-# LANGUAGE TemplateHaskell #-}
module Smten.Derive.Show (
   derive_showsPrec,
 ) where

import Language.Haskell.TH

-- | Derive the body of the showsPrec method of the Show class for the given
-- data type.
--
-- This can be used to auto derive the Show class. For example:
--  showsPrecFoo :: (Show a) => Int -> Foo a -> ShowS
--  showsPrecFoo = $(derive_showsPrec ''Foo)
--  instance (Show a) => Show (Foo a) where
--      showsPrec = showsPrecFoo
derive_showsPrec :: Name -> Q Exp
derive_showsPrec dnm = do
  -- \d x ->
  --    case x of
  --       (Foo1 a1 a2 ...) -> showParen (d > 10) (showString "Foo1"
  --                        . (showString " " . showsPrec 11 a1)
  --                        . (showString " " . showsPrec 11 a2)
  --       (Foo2) -> showString "Foo2"
  --       ...
  -- TODO: give a better error message if the name doesn't refer to a plain
  -- type constructor.
  -- TODO: support record constructors properly, not as normal constructors
  let showstr = VarE $ mkName "showString"
      mkcon :: Con -> Match
      mkcon (NormalC nm []) =
        let pat = ConP nm []
            body = AppE showstr (LitE (StringL $ nameBase nm))
        in Match pat (NormalB body) []
      mkcon (NormalC nm tys) =
        let dot :: Exp -> Exp -> Exp
            dot a b = AppE (AppE (VarE $ mkName ".") a) b

            mkarg :: Name -> Exp
            mkarg anm = (AppE showstr (LitE (StringL " "))) `dot`
                        (AppE (AppE (VarE $ mkName "showsPrec") (LitE $ IntegerL 11)) (VarE anm))

            argnms = [mkName $ "a" ++ show i | (i, _) <- zip [0..] tys] 
            pat = ConP nm [VarP a | a <- argnms]
            constr = AppE showstr (LitE (StringL $ nameBase nm))
            noparen = foldl dot constr (map mkarg argnms)
            body = AppE (AppE (VarE $ mkName "showParen")
                              (AppE (AppE (VarE $ mkName ">")
                                          (VarE $ mkName "d"))
                                    (LitE $ IntegerL 10))) noparen
        in Match pat (NormalB body) []
      mkcon (RecC nm tys) = mkcon (NormalC nm [(s, t) | (n, s, t) <- tys])
  TyConI (DataD _ _ _ cs _) <- reify dnm
  let body = CaseE (VarE $ mkName "x") (map mkcon cs)
  return $ LamE [VarP $ mkName "d", VarP $ mkName "x"] body

