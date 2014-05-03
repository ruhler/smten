
{-# LANGUAGE DeriveDataTypeable #-}

-- | The Smten Compiler
--
-- This executable is just a wrapper which properly invokes ghc with the
-- smten-plugin.

import Control.Monad.State
import Data.List
import Data.Version (showVersion)
import System.Environment
import System.Exit
import System.Process

import Paths_smten

usage :: String
usage = unlines [
    "Usage:",
    "  smten --make -o outfile infile.hs [options]",
    "",
    "Supported Options:",
    "  -?, --help           Print this usage information and exit",
    "  --version            Print the smten version and exit",
    "  -v, -vn              Enable verbose operation",
    "  -O                   Enable optimiziation",
    "  -prof                Enable profiling",
    "  -debug               Link with debug version of GHC runtime",
    "  -fprof-auto-top      Auto-add SCCs to top-level bindings",
    "  -idir1:dir2:...      Add dir1, dir2, etc. to import path",
    "  -hidir dir           Set directory for interface files",
    "  -odir dir            Set directory for object files",
    "  -rtsopts             Enable RTS options in generated executable",
    "  -fwarn-*             Turn on a standard GHC warning",
    "  -fno-warn-*          Turn off a standard GHC warning",
    ""]

data Mode = Help | Version | Make | Error String

data Options = Options {
    o_mode :: Mode,

    -- Options for stage 1 call to ghc.
    o_s1args :: [String],

    -- Options for stage 2 call to ghc.
    o_s2args :: [String]
}

type OptionsM = State Options

-- Add a list of stage 1 options.
s1 :: [String] -> OptionsM ()
s1 xs = modify $ \s -> s { o_s1args = xs ++ o_s1args s }

-- Add a list of stage 2 options.
s2 :: [String] -> OptionsM ()
s2 xs = modify $ \s -> s { o_s2args = xs ++ o_s2args s }

-- Set the mode.
smode :: Mode -> OptionsM ()
smode m = modify $ \s -> s { o_mode = m }

getopts :: [String] -> OptionsM ()
getopts al =
  case al of
    [] -> return ()
    ("--help" : tl) -> smode Help >> getopts tl
    ("-?" : tl) -> smode Help >> getopts tl
    ("--version" : tl) -> smode Version >> getopts tl
    ("--make" : tl) -> smode Make >> getopts tl
    ("-o" : f : tl) -> s2 ["-o", f] >> getopts tl
    ("-O" : tl) -> s2 ["-O"] >> getopts tl
    (v@('-':'v':_) : tl) -> s1 [v] >> s2 [v] >> getopts tl
    ("-prof" : tl) -> s1 ["-prof"] >> s2 ["-prof"] >> getopts tl
    ("-debug" : tl) -> s2 ["-debug"] >> getopts tl
    ("-fprof-auto-top" : tl) -> s1 ["-fprof-auto-top"] >> getopts tl
    ("-hidir" : f : tl) -> s1 ["-hidir", f] >> s2 ["-hidir", f] >> getopts tl
    ("-odir" : f : tl) -> s1 ["-odir", f] >> s2 ["-odir", f] >> getopts tl
    ("-rtsopts" : tl) -> s2 ["-rtsopts"] >> getopts tl
    (w@('-':'f':'w':'a':'r':'n':'-':_) : tl) -> s1 [w] >> getopts tl
    (w@('-':'f':'n':'o':'-':'w':'a':'r':'n':'-':_) : tl) -> s1 [w] >> getopts tl
    (x@('-':'i':_) : tl) -> s1 [x] >> s2 [x] >> getopts tl
    (('-':f) : _) -> smode (Error $ "unrecognized flag: " ++ show ('-':f))
    (f : tl) -> s1 [f] >> getopts tl

          

main :: IO ()
main = do
  args <- getArgs
  let opts = execState (getopts args) (Options Help [] [])

  case o_mode opts of
    Help -> putStrLn usage >> exitSuccess
    Version -> putStrLn $ "smten version " ++ showVersion version
    Error msg -> do
        putStrLn msg  
        putStrLn usage
        exitFailure
    Make -> do
      let s1 = ["--make", "-osuf", "smten_o",
                "-fplugin=Smten.Plugin.Plugin",
                "-XNoImplicitPrelude",
                "-O0", "-c"] ++ o_s1args opts
      putStrLn "Stage 1: Compiling using ghc with smten plugin"
      putStrLn $ "ghc " ++ show s1
      ec1 <- rawSystem "ghc" s1
      case ec1 of
        ExitSuccess -> return ()
        ExitFailure n -> error $ "State 1 smten compilation failed with: " ++ show n

      let s2 = ["--make", "Smten.Compiled.Main",
                "-main-is", "Smten.Compiled.Main.main"] ++ o_s2args opts
      putStrLn "State 2: Compiling generated haskell code without smten plugin"
      putStrLn $ "ghc " ++ show s2
      ec2 <- rawSystem "ghc" s2
      case ec2 of
        ExitSuccess -> return ()
        ExitFailure n -> error $ "State 2 smten compilation failed with: " ++ show n

