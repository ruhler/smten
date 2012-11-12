
{-# LANGUAGE PatternGuards #-}

module Seri.Elaborate.ToExpH (
  toExpH, stringEH,
    ) where

import Seri.Lambda
import Seri.Type.Sugar
import Seri.Type.SeriT
import Seri.Elaborate.ExpH
import Seri.ExpH.Sugar

-- | Translate an Exp to our HOAS ExpH representation
toExpH :: [(Sig, ExpH)] -> Exp -> ExpH
toExpH _ (LitE l) = LitEH l
toExpH _ (ConE s) = ConEH s
toExpH m (VarE s) | Just v <- lookup s m = v
toExpH m (VarE s) = VarEH s
toExpH m (AppE f xs) =
  let appeh :: ExpH -> Exp -> ExpH
      appeh f x = AppEH ES_None f (toExpH m x)
  in foldl appeh (toExpH m f) xs
toExpH m e | Just (Match [VarP s] b) <- deLamE e =   
  LamEH ES_None s $ \x -> toExpH ((s, x):m) b
toExpH m e@(LaceE ms@(Match [_] _ : _)) =
  let deSugarLace :: [(Sig, ExpH)] -> [Match] -> ExpH
      deSugarLace vars ms@(Match [p] b : _) =
        let tpat = typeof p
            tbody = typeof b
            terr = arrowsT [stringT, tbody]
            errv = VarEH (Sig (name "Prelude.error") terr)
            err = AppEH ES_None errv (stringEH "Case no match")

            depat :: [(Sig, ExpH)] -- ^ Variables in scope
                  -> ExpH -- ^ Argument to the case expression
                  -> Pat  -- ^ Pattern to match against
                  -> ([(Sig, ExpH)] -> ExpH) -- ^ Body if match succeeds   
                  -> ExpH -- ^ Default value on failure
                  -> ExpH
            depat vars arg (WildP {}) b _ = b vars
            depat vars arg (VarP s) b _ =
              let lam = LamEH ES_None s $ \x -> b ((s, x):vars)
              in AppEH ES_None lam arg
            depat vars arg (LitP l) b def =
              let lt = typeof l
                  eqt = arrowsT [lt, lt, boolT]
                  p = appsEH (VarEH (Sig (name "Prelude.==") eqt)) [LitEH l, arg]
              in ifEH p (b vars) def
            depat vars arg (ConP t n ps) b def =
              let k = Sig n (arrowsT ((map typeof ps) ++ [t]))
                  mkmatched :: [(Sig, ExpH)] -> [(Pat, ExpH)] -> ([(Sig, ExpH)] -> ExpH) -> ExpH -> ExpH
                  mkmatched vars [] b _ = b vars
                  mkmatched vars ((p, x):ps) b def =
                    let body = \vs -> mkmatched vs ps b def
                    in depat vars x p body def
  
                  mklams :: [(Sig, ExpH)] -> [Pat] -> [(Pat, ExpH)] -> ([(Sig, ExpH)] -> ExpH) -> ExpH -> ExpH
                  mklams vars [] ps body def = mkmatched vars ps body def
                  mklams vars (p:ps) ps' body def =
                    LamEH ES_None (Sig (name ("_cb")) (typeof p)) $ \x ->
                      mklams vars ps ((p, x):ps') body def
              in CaseEH ES_None arg k (mklams vars ps [] b def) def
            
            -- perform core desugaring of case statements.
            desugar :: ExpH -- ^ Argument to the case expression
                    -> [Match] -- ^ Set of matches
                    -> ExpH -- ^ The default value if no match
                    -> ExpH -- ^ The desugared expression
            desugar arg [Match [p] b] def = depat vars arg p (flip toExpH b) def
            desugar arg (m:ms) def = desugar arg [m] (desugar arg ms def)

        in LamEH (ES_Some WHNF) (Sig (name "_ca") tpat) $ \ca ->
              desugar ca ms err
  in deSugarLace m ms
toExpH m (LaceE ms) = toExpH m (sLaceE ms)

stringEH :: String -> ExpH
stringEH str = toExpH [] (stringE str)

