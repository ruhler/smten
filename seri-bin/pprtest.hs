
import Seri.Ppr
import Seri.Type
import Seri.Name

test :: (Monad m) => String -> Bool -> m ()
test msg p = if p then return () else error msg

testeq :: (Monad m, Show a, Eq a) => String -> a -> a -> m ()
testeq msg a b =
    if a == b
        then return ()
        else error $ msg ++ ": " ++ show a ++ " /= " ++ show b

main = do
    testeq "type app"
        "Foo Bar Sludge"
        (pretty (appsT (conT (name "Foo")) [conT (name "Bar"), conT (name "Sludge")]))

