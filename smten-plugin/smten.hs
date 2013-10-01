
{-# LANGUAGE DeriveDataTypeable #-}

-- | The Smten Compiler
--
-- This executable is just a wrapper which properly invokes ghc with the
-- smten-plugin.

import System.Cmd
import System.Exit
import Smten.Plugin.ParseArgs

main :: IO ()
main = do
  args <- parseArgs

  if sa_help args
     then putStrLn usage >> exitSuccess 
     else return ()

  let infile = sa_infile args
      outfile = sa_outfile args
      profile = sa_profile args
      optimize = sa_optimize args

  -- Step 1: Compile the code with -fplugin=Smten.Plugin.Plugin
  let s1args = [infile,
                 "--make", "-osuf", "smten_o", 
                 "-hisuf", "smten_hi",
                 "-prof",
                 "-fplugin=Smten.Plugin.Plugin",
                 "-XRebindableSyntax", "-XNoImplicitPrelude",
                 "-O0", "-c"]
  putStrLn "Stage 1: Compiling using ghc with smten plugin"
  putStrLn $ "ghc " ++ show s1args
  ec1 <- rawSystem "ghc" s1args
  case ec1 of
    ExitSuccess -> return ()
    ExitFailure n -> error $ "State 1 smten compilation failed with: " ++ show n

  -- Step 2: Compile the generated smten code.
  --   Set -main-is Smten.Compiled.Main.main
  --   Add -i<odir>
  let s2args = concat [["-o", outfile,
                        "Smten/Compiled/Main.hs",
                        "-main-is", "Smten.Compiled.Main.main"],
                       if profile then ["-prof"] else [],
                       if optimize then ["-O"] else []]

  putStrLn "State 2: Compiling generated haskell code without smten plugin"
  putStrLn $ "ghc " ++ show s2args
  ec2 <- rawSystem "ghc" s2args
  case ec2 of
    ExitSuccess -> return ()
    ExitFailure n -> error $ "State 2 smten compilation failed with: " ++ show n

