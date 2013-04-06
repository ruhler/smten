
module Smten.Name.Sugar (
    arrowN, unitN, trueN, falseN,
    tupleN, de_tupleN,
    unqualified, qualified, qualification,
    ) where

import Smten.Name.Name

import Control.Monad(guard)
import Data.List(genericReplicate, genericLength)

-- | The name of the (->) type constructor.
arrowN :: Name
arrowN = name "->"

unitN :: Name
unitN = name "()"

trueN :: Name
trueN = name "True"

falseN :: Name
falseN = name "False"

-- Generate the tuple name for given number of arguments.
tupleN :: (Integral n) => n -> Name
tupleN n = name $ "(" ++ genericReplicate (n-1) ',' ++ ")"

-- Check if a name is a tuple name. If so, returns the number of elements in
-- the tuple.
de_tupleN :: Name -> Maybe Integer
de_tupleN n = do
    let s = unname n
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


