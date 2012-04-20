
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Seri.Lib.Char where

import Seri

decltycon 0 ''Char

charB :: Builtin
charB =
  let mp _ = Nothing

      mt "Char" = Just "Prelude.Char"
      mt _ = Nothing
  in Builtin {
     mapprim = mp,
     maptype = mt,
     includes = empty
  }

