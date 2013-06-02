
import qualified Prelude as P
import Runtime.Prelude
import Runtime.Symbolic

runmain :: IO Unit -> P.IO ()
runmain (IO x) = x P.>> P.return ()

main :: P.IO ()
main = runmain P.$
  bind_io (run_symbolic (return Unit)) P.$ \r ->
    __caseJust r (\_ -> putChar (Char 'Y')) (putChar (Char 'N'))

