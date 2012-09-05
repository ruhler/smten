
import Prelude hiding ((<), (>))
import Seri.Lambda

import Seri.Enoch.Enoch
import Seri.Enoch.Prelude

q1 :: Query (Answer Integer)
q1 = do
    x <- free
    assert (x < 6)
    assert (x > 4)
    query x

main :: IO ()
main = do
    runQuery env q1 >>= (putStrLn . show)
    

