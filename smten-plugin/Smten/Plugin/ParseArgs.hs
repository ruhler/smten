
module Smten.Plugin.ParseArgs (
    SmtenArgs(..), parseArgs, usage,
    ) where

import System.Environment
import System.Exit

data SmtenArgs = SmtenArgs {
    sa_infile :: FilePath,
    sa_outfile :: FilePath,
    sa_profile :: Bool,
    sa_optimize :: Bool,
    sa_help :: Bool
} 

usage :: String
usage = "usage: smten -o outfile infile.hs [-prof] [-O]"

defargs :: SmtenArgs
defargs = SmtenArgs {
    sa_infile = "",
    sa_outfile = "a.out",
    sa_profile = False,
    sa_optimize = False,
    sa_help = False
}

update :: [String] -> SmtenArgs -> SmtenArgs
update xs args =
  case xs of
    [] -> args
    ("--help" : tl) -> args { sa_help = True }
    ("-prof" : tl) -> update tl (args { sa_profile = True })
    ("-O" : tl) -> update tl (args { sa_optimize = True })
    ("-o" : f : tl) -> update tl (args { sa_outfile = f })
    (f : _)
       | head f == '-' -> error $ "unrecognized flag: " ++ show f
       | sa_infile args == "" -> args { sa_infile = f }
       | otherwise -> error $ "duplicate inputfiles: " ++ sa_infile args ++ ", " ++ f
    _ -> error $ "malformed command line"

parseArgs :: IO SmtenArgs
parseArgs = do
    xs <- getArgs
    return (update xs defargs)

