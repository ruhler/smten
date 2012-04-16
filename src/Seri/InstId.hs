
module Seri.InstId (
    InstId(), noinst
    ) where

data InstId = NoInst
    deriving(Eq, Show)

noinst :: InstId
noinst = NoInst

