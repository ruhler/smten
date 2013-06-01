
import qualified Prelude as P
import Runtime.Prelude

runmain :: IO Unit -> P.IO ()
runmain (IO x) = x P.>> P.return ()

main :: P.IO ()
main = runmain P.$ putChar (Char 'Y')

