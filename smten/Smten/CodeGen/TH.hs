
module Smten.CodeGen.TH (declare_SmtenHS, derive_SmtenHS) where

import Language.Haskell.TH

-- class SmtenHSN m where
--   realizeN :: (SmtenHS a1, SmtenHS a2, ...) => Assignment -> m a1 a2 ... aN -> m a1 a2 ... aN
--   primitiveN :: (SmtenHS a1, SmtenHS a2, ...)
--                  => (Assignment -> m a1 a2 ... aN) -> m a1 a2 ... -> m a1 a2 ... aN
--   iteN :: (SmtenHS a1, SmtenHS a2, ...) => Bool -> a -> a -> a
--   sappN :: (SmtenHS a1, SmtenHS a2, ...) => (a -> b) -> a -> b
--   errorN :: (SmtenHS a1, SmtenHS a2, ...) => Prelude.String -> m a1 a2 ... aN
--   valueofN :: (SmtenHS a1, SmtenHS a2, ...) => m a1 a2 ... aN -> Prelude.Integer
declare_SmtenHS :: Integer -> Q [Dec]
declare_SmtenHS n = do
  let cls = mkName $ "SmtenHS" ++ show n
      tyvs = [PlainTV (mkName "m")]
      as = [mkName $ "a" ++ show i | i <- [1..n]]
      ctx = [ClassP (mkName "SmtenHS0") [VarT a] | a <- as]
      mas = foldl AppT (VarT $ mkName "m") (map VarT as)

      arrowT a b = AppT (AppT ArrowT a) b
      arrowsT = foldr1 arrowT
        
      relN = SigD (mkName $ "realize" ++ show n) $
                ForallT (map PlainTV as) ctx $
                  arrowsT [ConT $ mkName "Assignment", mas, mas]

      primN = SigD (mkName $ "primitive" ++ show n) $
                ForallT (map PlainTV as) ctx $
                  arrowsT [arrowsT [ConT $ mkName "Assignment", mas], mas, mas]

      sappctx = ctx ++ [ClassP (mkName "SmtenHS0") [VarT $ mkName "b"]]
      sappN = SigD (mkName $ "sapp" ++ show n) $
                ForallT (map PlainTV as ++ [PlainTV $ mkName "b"]) sappctx $
                 arrowsT [arrowsT [mas, VarT $ mkName "b"], mas, VarT $ mkName "b"]

      errN = SigD (mkName $ "error" ++ show n) $
                ForallT (map PlainTV as) ctx $
                 arrowsT [ConT $ mkName "Prelude.String", mas]

      iteN = SigD (mkName $ "ite" ++ show n) $ 
                   ForallT (map PlainTV as) ctx $
                     arrowsT [ConT (mkName "Bool"), mas, mas, mas]

      valueofN = SigD (mkName $ "valueof" ++ show n) $
                ForallT (map PlainTV as) ctx $
                  arrowsT [mas, ConT (mkName "Prelude.Integer")]
      vbody = AppE (VarE $ mkName "Prelude.error")
                   (LitE $ StringL "valueof on non-numeric type")
      vcls = Clause [] (NormalB vbody) []
      valueofdef = FunD (mkName $ "valueof" ++ show n) [vcls]

      methods = [relN, primN, sappN, errN, iteN, valueofN, valueofdef]
      classD = ClassD [] cls tyvs [] methods
  return [classD]
  
-- instance (SmtenHS(N+1) m, SMtenHS0 a) => SmtenHSN (m a) where
--   realizeN = realize(N+1)
--   primitiveN = primitive(N+1)
--   ...
derive_SmtenHS :: Integer -> Q [Dec]
derive_SmtenHS n = do
  let ctx = [
        ClassP (mkName $ "SmtenHS" ++ show (n+1)) [VarT $ mkName "m"],
        ClassP (mkName "SmtenHS0") [VarT $ mkName "a"]
        ]
      ty = AppT (ConT (mkName $ "SmtenHS" ++ show n))
                (AppT (VarT $ mkName "m") (VarT $ mkName "a"))
      relN = ValD (VarP (mkName $ "realize" ++ show n)) 
                  (NormalB $ VarE (mkName $ "realize" ++ show (n+1))) []
      sappN = ValD (VarP (mkName $ "sapp" ++ show n)) 
                  (NormalB $ VarE (mkName $ "sapp" ++ show (n+1))) []
      primN = ValD (VarP (mkName $ "primitive" ++ show n)) 
                  (NormalB $ VarE (mkName $ "primitive" ++ show (n+1))) []
      errN = ValD (VarP (mkName $ "error" ++ show n)) 
                  (NormalB $ VarE (mkName $ "error" ++ show (n+1))) []
      iteN = ValD (VarP (mkName $ "ite" ++ show n)) 
                  (NormalB $ VarE (mkName $ "ite" ++ show (n+1))) []
      valueofN = ValD (VarP (mkName $ "valueof" ++ show n)) 
                  (NormalB $ VarE (mkName $ "valueof" ++ show (n+1))) []
      instD = InstanceD ctx ty [relN, sappN, primN, errN, iteN, valueofN]
  return [instD]

