
{-# LANGUAGE TemplateHaskell #-}

module Seri.Quoter (
    s
    ) where

import Data.Char(ord)
import Data.List(nub)
import Text.Parsec hiding (token)
import Language.Haskell.Meta.Parse(parseExp)

import Language.Haskell.TH.Quote
import Language.Haskell.TH

import qualified Seri.IR as SIR
import qualified Seri.Typed as S
import Seri.Declarations


-- Run a parser
-- Fails if there is a parse error.
run :: (Monad m) => Parser a -> String -> m a
run p str 
  = case (runParser p initialUserState "here" str) of
        Left err -> fail $ show err
        Right x -> return x

data UserState = UserState {
    boundnames :: [SIR.Name],
    freenames :: [SIR.Name]
}

initialUserState = UserState [] []

type Parser = Parsec String UserState

bindname :: SIR.Name -> Parser ()
bindname nm = modifyState (\us -> us { boundnames = (nm:(boundnames us)) })

unbindname :: SIR.Name -> Parser ()
unbindname nm = do
    UserState (n:names) fn <- getState
    if (n /= nm) 
        then fail $ "unbindname '" ++ nm ++ "' doesn't match expected '" ++ n ++ "'"
        else putState $ UserState names fn

getBound :: Parser [SIR.Name]
getBound = getState >>= return . boundnames

freename :: SIR.Name -> Parser ()
freename nm = modifyState (\us -> us { freenames = (nm:(freenames us)) })

clearFree :: Parser ()
clearFree = modifyState (\us -> us { freenames = [] })

getFree :: Parser [SIR.Name]
getFree = getState >>= return . freenames

apply :: Name -> [Exp] -> Exp
apply n exps = foldl AppE (VarE n) exps

infixp :: Name -> (Exp -> Exp -> Exp)
infixp nm = (\a b -> apply 'S.infixE [VarE nm, a, b])

atom :: Parser Exp
atom = eth <|> elam <|> eparen <|> einteger <|> try eif <|> try ename

appls :: Parser Exp
appls = atom `chainl1` eapp

mults :: Parser Exp
mults = appls `chainl1` emul

adds :: Parser Exp
adds = mults `chainl1` (eadd <|> esub)

lts :: Parser Exp
lts = adds `chainl1` elt

expr :: Parser Exp
expr = lts

-- top level parser, skips initial whitespace, force match at eof.
top :: Parser a -> Parser a
top p = do
    many space
    x <- p
    eof
    return x

-- A string token followed by optional space.
token :: String -> Parser String
token x = do
    string x
    many space
    return x

eparen :: Parser Exp
eparen = do
    token "("
    x <- expr
    token ")"
    return x

eif :: Parser Exp
eif = do
    token "if"
    p <- expr
    token "then"
    tb <- expr
    token "else"
    tf <- expr
    return $ apply 'S.ifE [p, tb, tf]

einteger :: Parser Exp
einteger = do
    x <- integer
    many space
    return $ apply 'S.integerE [LitE (IntegerL x)]

integer :: Parser Integer
integer = do 
    digits <- many1 digit
    return $ digitstoint 0 digits

digitstoint :: Integer -> [Char] -> Integer
digitstoint acc [] = acc
digitstoint acc (x:xs) = digitstoint (acc*10 + (fromIntegral $ (ord x - ord '0'))) xs

einfix :: String -> Name -> Parser (Exp -> Exp -> Exp)
einfix str name = do
    token str
    return $ infixp name
    
eadd :: Parser (Exp -> Exp -> Exp)
eadd = einfix "+" 'S.addP

esub :: Parser (Exp -> Exp -> Exp)
esub = einfix "-" 'S.subP

elt :: Parser (Exp -> Exp -> Exp)
elt = einfix "<" 'S.ltP

emul :: Parser (Exp -> Exp -> Exp)
emul = einfix "*" 'S.mulP

eapp :: Parser (Exp -> Exp -> Exp)
eapp = do
    return $ (\a b -> apply 'S.appE [a, b])

elam :: Parser Exp
elam = do
    char '\\'
    nm <- name
    token "->"
    bindname nm
    body <- expr
    unbindname nm
    return $ apply 'S.lamE [LitE (StringL nm), LamE [VarP $ mkName nm] body]

keyword = ["if", "then", "else"]

ename :: Parser Exp
ename = do
    nm <- name
    notFollowedBy (token "::")
    bound <- getBound
    case () of
        _ | nm `elem` keyword -> fail $ "keyword '" ++ nm ++ "' used as a variable"
        _ | nm `elem` bound -> return $ VarE (mkName nm)
        _ -> do
            freename nm
            return $ apply 'S.varE_typed [VarE (name_P nm), LitE (StringL nm)]

name :: Parser SIR.Name
name = do
    x <- many1 alphaNum
    many space
    return x

-- Parse a template haskell slice
-- @(...)
-- Where ... is haskell code with properly nested parentheses.
eth :: Parser Exp
eth = do
    string "@("
    str <- strtoclose 1
    many space
    case parseExp str of
        Right x -> return $ x
        Left err -> fail err

strtoclose :: Integer -> Parser String
strtoclose n = do
    c <- anyChar
    case (c, n) of
        (')', 1) -> return ""
        (')', _) -> do
            s <- strtoclose (n-1)
            return (c:s)
        ('(', _) -> do
            s <- strtoclose (n+1)
            return (c:s)
        (_, _) -> do
            s <- strtoclose n
            return (c:s)


-- parse a value declaration. Syntax is:
--      foo :: MyType
--      foo = myval
dval :: Parser [Dec]
dval = do
    n <- name
    token "::"
    t <- type_
    n' <- name
    if (n /= n')
        then fail $ "type and exp have different names in decl: " ++ n ++ " vs. " ++ n'
        else return ()
    token "="
    clearFree
    e <- expr
    free <- getFree

    let vns = tvarnames t
    let ptt = (AppT (ConT ''S.TypedExp) t)
    let inctx t = ForallT (map PlainTV vns)
                   (map (\x -> ClassP ''S.SeriType [VarT x]) vns)
                   t
    let pta = if null vns then ptt else inctx ptt
    return $ declval n pta e free

-- Return a list of all the variable type names in the given type.
tvarnames :: Type -> [Name]
tvarnames (ForallT _ _ t) = tvarnames t
tvarnames (VarT nm) = [nm]
tvarnames (AppT a b) = nub $ (tvarnames a) ++ (tvarnames b)
tvarnames t = []

-- Parse a bunch of declarations
decls :: Parser [Dec]
decls = many1 dval >>= return . concat

tcon :: Parser Type
tcon = do
    x <- upper
    xs <- many alphaNum
    many space
    return $ ConT (mkName (x:xs))

tvar :: Parser Type
tvar = do
    c <- oneOf "abcd"
    many space
    return $ (VarT $ mkName [c])

tparen :: Parser Type
tparen = do
    token "("
    x <- type_
    token ")"
    return x

tatom :: Parser Type
tatom = tparen <|> tcon <|> tvar

tappls :: Parser Type
tappls = tatom `chainl1` tapp

tarrows :: Parser Type
tarrows = tappls `chainl1` tarrow

tapp :: Parser (Type -> Type -> Type)
tapp = return AppT

tarrow :: Parser (Type -> Type -> Type)
tarrow = do
    token "->"
    return $ \at bt -> AppT (AppT ArrowT at) bt

type_ :: Parser Type
type_ = tarrows

s :: QuasiQuoter 
s = QuasiQuoter qexp qpat qtype qdec

qexp :: String -> Q Exp
qexp = run (top expr)

qpat :: String -> Q Pat
qpat = error $ "Seri pattern quasi-quote not supported"

qtype :: String -> Q Type
qtype = error $ "Seri type quasi-quote not supported"

qdec :: String -> Q [Dec]
qdec = run (top decls)


