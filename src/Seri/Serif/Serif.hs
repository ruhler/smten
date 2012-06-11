
module Seri.Serif.Serif (serif)
  where


import qualified Language.Haskell.TH as H
import Seri.Lambda
import Seri.Utils.Ppr

serif :: [Dec] -> [H.Dec]
serif = concat . map mkdec  

mkdec :: Dec -> [H.Dec]
mkdec d = error $ "TODO: mkdec: " ++ render (ppr d)

