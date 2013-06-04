
module Smten.CodeGen.TH (declare_SmtenHS, derive_SmtenHS) where

import Language.Haskell.TH

-- class SmtenHSN m where
--   muxN :: (SmtenHS a1, SmtenHS a2, ...) => Bool -> m a1 a2 ... aN -> m a1 a2 ... aN -> m a1 a2 ... aN
--   realizeN :: (SmtenHS a1, SmtenHS a2, ...) => [(FreeID, Dynamic)] -> m a1 a2 ... aN -> m a1 a2 ... aN
declare_SmtenHS :: Integer -> Q [Dec]
declare_SmtenHS n = do
  let cls = mkName $ "SmtenHS" ++ show n
      tyvs = [PlainTV (mkName "m")]
      as = [mkName $ "a" ++ show i | i <- [1..n]]
      ctx = [ClassP (mkName "SmtenHS0") [VarT a] | a <- as]
      mas = foldl AppT (VarT $ mkName "m") (map VarT as)

      arrowT a b = AppT (AppT ArrowT a) b
      arrowsT = foldr1 arrowT

      muxN = SigD (mkName $ "mux" ++ show n) $
                ForallT (map PlainTV as) ctx $
                  arrowsT [ConT (mkName "Bool"), mas, mas, mas]
        
      relN = SigD (mkName $ "realize" ++ show n) $
                ForallT (map PlainTV as) ctx $
                  arrowsT [AppT ListT (foldl AppT (TupleT 2) [ConT $ mkName "FreeID", ConT $ mkName "Dynamic"]), mas, mas]
      classD = ClassD [] cls tyvs [] [muxN, relN]
  return [classD]
  
-- instance (SmtenHS(N+1) m, SMtenHS0 a) => SmtenHSN (m a) where
--   muxN = mux(N+1)
--   realizeN = realize(N+1)
derive_SmtenHS :: Integer -> Q [Dec]
derive_SmtenHS n = do
  let ctx = [
        ClassP (mkName $ "SmtenHS" ++ show (n+1)) [VarT $ mkName "m"],
        ClassP (mkName "SmtenHS0") [VarT $ mkName "a"]
        ]
      ty = AppT (ConT (mkName $ "SmtenHS" ++ show n))
                (AppT (VarT $ mkName "m") (VarT $ mkName "a"))
      muxN = ValD (VarP (mkName $ "mux" ++ show n)) 
                  (NormalB $ VarE (mkName $ "mux" ++ show (n+1))) []
      relN = ValD (VarP (mkName $ "realize" ++ show n)) 
                  (NormalB $ VarE (mkName $ "realize" ++ show (n+1))) []
      instD = InstanceD ctx ty [muxN, relN]
  return [instD]
