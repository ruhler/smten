
module Smten.Name.Sugar (
    arrowN, unitN, trueN, falseN, charN, integerN, bitN,
    tupleN, de_tupleN,
    listN, consN, nilN,
    boolN,
    errorN, ioN, bindN, nobindN,
    unqualified, qualified, qualification, isqualified,
    ) where

import Smten.Name.Name

import Control.Monad(guard)
import Data.List(genericReplicate, genericLength)

preludeN :: Name
preludeN = name "Prelude"

-- | The name of the (->) type constructor.
arrowN :: Name
arrowN = qualified preludeN $ name "->"

unitN :: Name
unitN = qualified preludeN $ name "()"

boolN :: Name
boolN = qualified preludeN $ name "Bool"

trueN :: Name
trueN = qualified preludeN $ name "True"

falseN :: Name
falseN = qualified preludeN $ name "False"

charN :: Name
charN = qualified preludeN $ name "Char"

integerN :: Name
integerN = qualified preludeN $ name "Integer"

listN :: Name
listN = qualified preludeN $ name "[]"

consN :: Name
consN = qualified preludeN $ name ":"

nilN :: Name
nilN = listN

errorN :: Name
errorN = qualified preludeN $ name "error"

nobindN :: Name
nobindN = qualified preludeN $ name ">>"

bindN :: Name
bindN = qualified preludeN $ name ">>="

bitN :: Name
bitN = name "Smten.Bit.Bit"

ioN :: Name
ioN = qualified preludeN $ name "IO"

-- Generate the tuple name for given number of arguments.
tupleN :: (Integral n) => n -> Name
tupleN n = qualified preludeN $ name $ "(" ++ genericReplicate (n-1) ',' ++ ")"

-- Check if a name is a tuple name. If so, returns the number of elements in
-- the tuple.
de_tupleN :: Name -> Maybe Integer
de_tupleN n = do
    let s = unname (unqualified n)
    guard $ length s > 2
    guard $ head s == '('
    guard $ last s == ')'
    let mid = init (tail s)
    guard $ all (== ',') mid
    return (genericLength mid + 1)

-- | Return the unqualified part of the given name.
-- For example: unqualified "Foo.Bar.sludge" returns "sludge"
unqualified :: Name -> Name
unqualified n = 
  case (span (/= '.') (unname n)) of
    (_, []) -> n
    ([], ['.']) -> name "."
    (_, '.':xs) -> unqualified (name xs)

-- | Return the qualification on the given name.
-- For example: qualification "Foo.Bar.sludge" returns "Foo.Bar"
qualification :: Name -> Name
qualification n =
  case (span (/= '.') (unname n)) of
    (_, []) -> name ""
    ([], ['.']) -> name ""
    (x, '.':xs) -> 
        let qxs = qualification (name xs)
        in if nnull qxs
             then name x
             else qualified (name x) qxs

-- | Make a qualified name.
-- For example: qualified "Foo.Bar" "sludge" returns "Foo.Bar.sludge"
qualified :: Name -> Name -> Name
qualified a b = a `nappend` name "." `nappend` b

isqualified :: Name -> Bool
isqualified = not . nnull . qualification

