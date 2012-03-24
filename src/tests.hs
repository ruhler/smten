
import Seri
import SeriPrint
import Elaborate
import SeriHaskell
import CPrint
import qualified C

-- foo: (\x = x*x + 3*x + 2) 5
foo :: Exp
foo =
 let x = VarE IntegerT "x"
     body = AddE (AddE (MulE x x) (MulE (IntegerE 3) x)) (IntegerE 2)
     lam = LamE IntegerT "x" body
 in AppE (ArrowT IntegerT IntegerT) lam (IntegerE 5)

fooc :: (C.Dec, C.Exp)
fooc = 
  let x = C.VarE "x"
      body = C.AddE (C.AddE (C.MulE x x) (C.MulE (C.IntE 3) x)) (C.IntE 2)
      dec = C.FunD "l1" [(C.IntT, "x")] C.IntT [C.ReturnS body]
      exp = C.AppE (C.VarE "l1") [C.IntE 5]
  in (dec, exp)

main :: IO ()
main = do
    putStrLn $ show foo
    putStrLn $ show (elaborate foo)
    putStrLn $ show (ppr foo)
    putStrLn $ show (ppr (haskell foo))
    let (cd, ce) = fooc
    putStrLn $ show (ppr cd, ppr ce)

