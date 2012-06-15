
import Seri.Haskell
import Seri.Serif.Serif

main :: IO ()
main = putStrLn $ unlines [
                "{-# LANGUAGE ExplicitForAll #-}",
                "{-# LANGUAGE MultiParamTypeClasses #-}",
                show (ppr prelude)
                ]

