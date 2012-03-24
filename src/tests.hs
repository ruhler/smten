
import Seri
import SeriPrint
import Elaborate
import SeriHaskell
import CPrint
import qualified C
import SeriC
import SeriTypeCheck

-- foo: (\x = x*x + 3*x + 2) 5
foo :: Exp
foo =
 let x = VarE IntegerT "x"
     body = AddE (AddE (MulE x x) (MulE (IntegerE 3) x)) (IntegerE 2)
     lam = LamE (ArrowT IntegerT IntegerT) "x" body
 in AppE (ArrowT IntegerT IntegerT) lam (IntegerE 5)

fooc :: ([C.Dec], C.Exp)
fooc = c foo

main :: IO ()
main = do
    tfoo <- typecheck foo
    putStrLn $ show tfoo

    putStrLn $ show foo
    putStrLn $ show (elaborate foo)
    putStrLn $ show (ppr foo)
    putStrLn $ show (ppr (haskell foo))
    let (cd, ce) = fooc
    putStrLn $ show (ppr cd, ppr ce)

