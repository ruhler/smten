
import qualified Prelude as P
import Runtime.Prelude
import Runtime.Symbolic

-- runtime lib runmain
runmain :: IO Unit__ -> P.IO ()
runmain x = x P.>> P.return ()

main :: P.IO ()
main = runmain P.$
  bind_io (run_symbolic (return (S__c Unit__))) P.$ \r ->
    __caseJust r (\_ -> putChar 'Y') (putChar 'N')

