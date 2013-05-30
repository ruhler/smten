
import State
import qualified Map

type Counts = Map.Map

addword :: String -> State ()
addword w = do
  m <- (get :: State Counts)
  case Map.lookup w m of
     Just v -> put $ Map.insert w (v+1) m
     Nothing -> put $ Map.insert w 1 m

output :: (String, Integer) -> IO ()
output (s, i) = putStrLn $ s ++ ": " ++ show i

outputstr :: String -> IO ()
outputstr s = putStrLn s

main :: IO ()
main = do
    text <- getContents
    let lns = lines text
        wds = concatMap words lns
        counts = execState (mapM addword wds) Map.empty
    mapM_ output (Map.toList counts)

