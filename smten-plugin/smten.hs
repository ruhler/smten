
-- | The Smten Compiler
--
-- This executable is just a wrapper which properly invokes ghc with the
-- smten-plugin.

import System.Cmd
import System.Environment
import System.Exit

lookuparg :: String -> [String] -> Maybe String
lookuparg k m = 
  case dropWhile ((/=) k) m of
     (_:x:_) -> Just x
     _ -> Nothing

getfiles :: [String] -> [String]
getfiles [] = []
getfiles (x:xs) 
  | head x == '-' = getfiles (drop 1 xs)
  | otherwise = x:xs

usage :: String
usage = "Usage: smten -o outfile infile.hs"

main :: IO ()
main = do
  args <- getArgs

  if "--help" `elem` args
     then putStrLn usage >> exitSuccess
     else return ()

  let -- Get the name of output executable to generate.
      ofile = case lookuparg "-o" args of
                Just v -> v
                Nothing -> "a.out"

      -- Get the name of the input main file.
      ifile = case getfiles args of
                [v] -> v
                [] -> fail $ "no input file.\n" ++ usage
                _ -> fail $ "duplicate input files.\n" ++ usage

  -- Step 1: Compile the code with -fplugin=Smten.Plugin.Plugin
  putStrLn $ "smten got args ifile = " ++ ifile ++ ", ofile = " ++ ofile
  let s1args = [ifile,
                 "--make", "-osuf", "smten_o", 
                 "-hisuf", "smten_hi",
                 "-prof",
                 "-fplugin=Smten.Plugin.Plugin",
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
  let s2args = ["-o", ofile, "Smten/Compiled/Main.hs", "-main-is", "Smten.Compiled.Main.main"]
  putStrLn "State 2: Compiling generated haskell code without smten plugin"
  putStrLn $ "ghc " ++ show s2args
  ec2 <- rawSystem "ghc" s2args
  case ec2 of
    ExitSuccess -> return ()
    ExitFailure n -> error $ "State 2 smten compilation failed with: " ++ show n

