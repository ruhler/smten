
import qualified Prelude as P
import Runtime.Prelude
import Runtime.Symbolic

runmain :: IO Unit -> P.IO ()
runmain (Concrete1 x) = x P.>> P.return ()

-- test sharing leak in Assert translation
query :: Symbolic Char
query = (>>=) free_Bool P.$ \p ->
    let x = p && (p && (p && p))
        y = x || x
    in __caseTrue y (return (Concrete 'Y')) fail_symbolic

-- test sharing leak in case pushing
query1 :: Symbolic Char
query1 = (>>=) free_Bool P.$ \p ->
         (>>=) free_Bool P.$ \q ->
          let a = __mkJust q
              x = __caseTrue p (__caseTrue q __mkNothing a) a
          in __caseTrue (__caseJust x (\v -> v && (v && (v && not v))) __mkTrue)
                  (return (Concrete 'Y'))
                  fail_symbolic
                    

main :: P.IO ()
main = runmain P.$
  bind_io (run_symbolic query1) P.$ \r ->
    __caseJust r (\v -> putChar v) (putChar (Concrete 'N'))

