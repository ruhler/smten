
module Seri (seridir) where

import Paths_seri
    
-- | Directory where the Seri library files (.sri) are installed.
seridir :: IO FilePath
seridir = getDataDir

