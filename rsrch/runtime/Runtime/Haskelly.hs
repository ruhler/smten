
{-# LANGUAGE MultiParamTypeClasses #-}

module Runtime.Haskelly (
    Haskelly(..),
    ) where

import Data.Maybe(fromMaybe)

class Haskelly h s where
    fromHaskell :: h -> s
    toHaskell :: s -> Maybe h

instance (Haskelly ha sa, Haskelly hb sb) => Haskelly (ha -> hb) (sa -> sb) where
    fromHaskell hf sx =
      let hx = fromMaybe (error "fromHaskell (->)") (toHaskell sx)
      in fromHaskell (hf hx)

    toHaskell sf = return $ \hx ->
        let sx = fromHaskell hx
        in fromMaybe (error "toHaskell (->)") (toHaskell (sf sx))

