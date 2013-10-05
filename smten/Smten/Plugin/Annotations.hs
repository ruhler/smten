
{-# LANGUAGE DeriveDataTypeable #-}

module Smten.Plugin.Annotations (PrimitiveModule(..)) where

import Data.Typeable
import Data.Data

-- | Module annotation indicating a module is a primitive module, and so no
-- smten code generation should be performed on it. To use this annotation,
-- add the following line to the source code of your module after importing 
-- \"Smten.Plugin.Annotations\":
--  
-- >   ANN module PrimitiveModule 
data PrimitiveModule = PrimitiveModule 
    deriving (Typeable, Data, Show)

