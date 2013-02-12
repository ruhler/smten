
module Smten (smtendir) where

import Paths_smten
    
-- | Directory where the Smten library files (.smtn) are installed.
smtendir :: IO FilePath
smtendir = getDataDir

