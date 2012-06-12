
import Seri.Lambda
import Seri.Utils.Ppr
import Seri.Target.Monomorphic.Monomorphic

main :: IO ()
main = do
    let (input, output) = (getContents, putStrLn)

    polytext <- input
    poly <- parseDecs polytext
    let Just (_, e) = lookupvar (mkenv poly (VarE (Sig "main" undefined) Declared))
    let mono = monomorphic (mkenv poly e)
    output $ render (ppr mono)

