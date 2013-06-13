
module Smten.CodeGen.TH (declare_SmtenHS, derive_SmtenHS) where

import Language.Haskell.TH

-- class SmtenHSN m where
--   realizeN :: (SmtenHS a1, SmtenHS a2, ...) => Assignment -> m a1 a2 ... aN -> m a1 a2 ... aN
--   casesN :: (SmtenHS a1, SmtenHS a2, ..., SmtenHS b) => m a1 a2 ... aN -> Cases (m a1 a2 ...)
--   primitiveN :: (SmtenHS a1, SmtenHS a2, ..., SmtenHS b) => (Assignment -> m a1 a2 ... aN)
--                                                          -> Cases (m a1 a2 ...) -> m a1 a2 ... aN
declare_SmtenHS :: Integer -> Q [Dec]
declare_SmtenHS n = do
  let cls = mkName $ "SmtenHS" ++ show n
      tyvs = [PlainTV (mkName "m")]
      as = [mkName $ "a" ++ show i | i <- [1..n]]
      ctx = [ClassP (mkName "SmtenHS0") [VarT a] | a <- as]
      mas = foldl AppT (VarT $ mkName "m") (map VarT as)
      css = AppT (ConT (mkName "Cases")) mas

      arrowT a b = AppT (AppT ArrowT a) b
      arrowsT = foldr1 arrowT
        
      relN = SigD (mkName $ "realize" ++ show n) $
                ForallT (map PlainTV as) ctx $
                  arrowsT [ConT $ mkName "Assignment", mas, mas]

      casN = SigD (mkName $ "cases" ++ show n) $
                ForallT (map PlainTV as) ctx $
                  arrowsT [mas, css]

      primN = SigD (mkName $ "primitive" ++ show n) $
                ForallT (map PlainTV as) ctx $
                  arrowsT [arrowsT [ConT $ mkName "Assignment", mas], css, mas]

      casetrue = SigD (mkName $ "__caseTrue" ++ show n) $ 
                   ForallT (map PlainTV as) ctx $
                     arrowsT [ConT (mkName "Bool"), mas, mas, mas]

      rzs = [foldl1 AppE [VarE $ mkName v | v <- ["realize0", "m", x]]
                | x <- ["x", "y", "n"]]
      rval = LamE [VarP $ mkName "m"] $ foldl AppE (VarE $ mkName "__caseTrue0") rzs
      cval = foldl1 AppE [VarE $ mkName "switch",
                          VarE $ mkName "x",
                          AppE (VarE $ mkName "cases0") (VarE $ mkName "y"),
                          AppE (VarE $ mkName "cases0") (VarE $ mkName "n")]
      ctprim = foldl1 AppE [VarE $ mkName "primitive0", rval, cval]
      ctbody = CaseE (VarE $ mkName "x") [
                  Match (ConP (mkName "True") []) (NormalB (VarE $ mkName "y")) [],
                  Match (ConP (mkName "False") []) (NormalB (VarE $ mkName "n")) [],
                  Match (WildP) (NormalB ctprim) []]
      ctcls = Clause [VarP $ mkName n | n <- ["x", "y", "n"]] (NormalB ctbody) []
      casetruedef = FunD (mkName $ "__caseTrue" ++ show n) [ctcls]

      methods = [relN, casN, primN, casetrue, casetruedef]
      classD = ClassD [] cls tyvs [] methods
  return [classD]
  
-- instance (SmtenHS(N+1) m, SMtenHS0 a) => SmtenHSN (m a) where
--   realizeN = mux(N+1)
--   casesN = realize(N+1)
--   primitiveN = strict_app(N+1)
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
      casesN = ValD (VarP (mkName $ "cases" ++ show n)) 
                  (NormalB $ VarE (mkName $ "cases" ++ show (n+1))) []
      primN = ValD (VarP (mkName $ "primitive" ++ show n)) 
                  (NormalB $ VarE (mkName $ "primitive" ++ show (n+1))) []
      casetrueN = ValD (VarP (mkName $ "__caseTrue" ++ show n)) 
                  (NormalB $ VarE (mkName $ "__caseTrue" ++ show (n+1))) []
      instD = InstanceD ctx ty [relN, casesN, primN, casetrueN]
  return [instD]

