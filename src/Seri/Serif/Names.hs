
module Seri.Serif.Names (
    mknm, vnm, vpnm, clsnm, tyctnm, iidnm, mtynm, cvnm,
    ) where

import qualified Language.Haskell.TH as H
import Seri.Lambda.IR

prefixed :: String -> Name -> H.Name
prefixed p n = H.mkName (p ++ (desymbol n))

mknm :: Name -> H.Name
mknm = prefixed ""

vnm :: Name -> H.Name
vnm = prefixed "_serifP_"

vpnm :: Name -> H.Name
vpnm n = H.mkName ("p_" ++ n)

clsnm :: Name -> H.Name
clsnm = prefixed "SeriClass_"

tyctnm :: Name -> H.Name
tyctnm = prefixed "_serifS_"

iidnm :: Name -> H.Name
iidnm = prefixed "_serifI_"

cvnm :: Name -> H.Name
cvnm = prefixed "_serifK_"

mtynm :: Name -> H.Name
mtynm = prefixed "_serifT_"

desymbol :: String -> String
desymbol s =
  let syms = [
          ('!', "__bang"), ('#', "__hash"), ('$', "__dollar"),
          ('%', "__percent"), ('&', "__amp"), ('*', "__star"),
          ('+', "__plus"), ('.', "__dot"), ('/', "__slash"),
          ('<', "__lt"), ('=', "__eq"), ('>', "__gt"),
          ('?', "__ques"), ('@', "__at"), ('\\', "__bslash"),
          ('^', "__hat"), ('-', "__minus"), ('(', "__oparen"),
          (',', "__comma"), (')', "__cparen"), (':', "__colon"),
          ('[', "__obracket"), (']', "__cbracket")
          ]
    
      ds :: Char -> String
      ds c = case lookup c syms of
                Just str -> str
                Nothing -> [c]
   in concat . map ds $ s

