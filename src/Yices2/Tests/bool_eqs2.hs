
import qualified Yices2.Yices2 as Y
import Yices2.Syntax

query :: [Command]
query = [
    Define "a" BoolT Nothing,
    Define "b" BoolT Nothing,
    Define "c" BoolT Nothing,
    Define "d" BoolT Nothing,
    Define "e" BoolT Nothing,
    Assert (FunctionE (varE "=") [
        varE "a", FunctionE (varE "or") [varE "b", varE "c"]]),
    Assert (FunctionE (varE "=") [
        varE "d", FunctionE (varE "and") [varE "b", varE "c"]]),
    Assert (FunctionE (varE "=") [varE "a", varE "d"]),
    Check,
    ShowModel,
    Assert (FunctionE (varE "=") [
        varE "e", FunctionE (varE "xor") [varE "b", varE "c"]]),
    Assert (FunctionE (varE "=") [varE "e", varE "d"]),
    Check,
    ShowModel,
    Assert (varE "d"),
    Check
    ]

main :: IO ()
main = do
    putStrLn "Pretty:"
    putStrLn $ (pretty query)

    putStrLn "Running..."
    Y.init
    ctx <- Y.mkctx
    mapM (Y.run ctx) query
    Y.exit

    putStrLn "Done"

