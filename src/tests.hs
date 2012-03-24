
import Seri
import Elaborate

-- foo: (\x = x*x + 3*x + 2) 5
foo :: Exp
foo =
 let x = VarE IntegerT "x"
     body = AddE (AddE (MulE x x) (MulE (IntegerE 3) x)) (IntegerE 2)
     lam = LamE IntegerT "x" body
 in AppE (ArrowT IntegerT IntegerT) lam (IntegerE 5)

main :: IO ()
main = do
    putStrLn $ show foo
    putStrLn $ show (elaborate foo)

