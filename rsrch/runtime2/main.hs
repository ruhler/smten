
import qualified Prelude as P
import Runtime.Prelude
import Runtime.Symbolic

runmain :: IO Unit -> P.IO ()
runmain (IO x) = x P.>> P.return ()

query :: Symbolic Char
query = (>>=) free_Bool P.$ \p ->
    let x = p && (p && (p && p))
        y = x || x
    in __caseTrue y (return (Char 'Y')) fail_symbolic

main :: P.IO ()
main = runmain P.$
  bind_io (run_symbolic query) P.$ \r ->
    __caseJust r (\v -> putChar v) (putChar (Char 'N'))

