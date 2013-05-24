
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}

import System.IO.Unsafe
import Data.IORef

import Debug.Trace

import Smten.Name
import Smten.Type
import Smten.Sig
import Smten.Lit
import Smten.Ppr

newtype EID = EID Integer
    deriving (Eq, Ord)

-- ExpH_Value represents a symbolic expression evaluated to weak head normal
-- form, or a thunk.
data ExpH_Value =
            -- | Literal characters and integers
            LitEH Lit

            -- | Fully applied data constructors.
            -- The Type field is the type of the fully applied constructor.
          | ConEH Name Type [ExpH]

            -- | Primitive symbolic varibles.
            -- Current types supported are: Bool, Integer, Bit
          | VarEH Sig

            -- | Fully applied primitive functions
            -- | The Type field is the type of the fully applied primitive.
          | PrimEH Name Type ([ExpH] -> ExpH) [ExpH]
         
          -- | LamEH s t f:
          --    s - name and type of the function argument. 
          --        The name is for debugging purposes only.
          --    t - the return type of the function
          --    f - the haskell representation of the function.
          | LamEH Sig Type (ExpH -> ExpH)

          -- | Conditional expressions.
          | IfEH Type ExpH ExpH ExpH

          -- | Explicit _|_
          | ErrorEH Type String

data ExpH = ExpH {
    eid :: EID,
    force :: ExpH_Value
}

integerEH :: Integer -> ExpH
integerEH = litEH . integerL 

de_integerEH :: ExpH -> Maybe Integer
de_integerEH e = do
    l <- de_litEH e
    de_integerL l

litEH :: Lit -> ExpH
litEH = exph . LitEH

de_litEH :: ExpH -> Maybe Lit
de_litEH e
 | LitEH l <- force e = Just l
 | otherwise = Nothing


instance Show ExpH where
    show = show . force

instance Show ExpH_Value where
    show (LitEH l) = pretty l
    show (ConEH n _ xs) = pretty n ++ " " ++ show xs
    show (VarEH s) = pretty s
    show (PrimEH n _ _ xs) = pretty n ++ " " ++ show xs
    show (LamEH s _ _) = "\\" ++ pretty s ++ " -> ..."
    show (IfEH _ p a b) = "if " ++ show p ++ " then " ++ show a ++ " else " ++ show b
    show (ErrorEH _ s) = "error " ++ show s

trueEH :: ExpH
trueEH = aconEH trueN boolT []

falseEH :: ExpH
falseEH = aconEH falseN boolT []

-- | Boolean expression
boolEH :: Bool -> ExpH
boolEH True = trueEH
boolEH False = falseEH

appEH :: ExpH -> ExpH -> ExpH
appEH f x
 | LamEH _ _ g <- force f = g x
 | ErrorEH ft s <- force f =
     let Just (_, t) = de_arrowT ft
     in errorEH t s
 | otherwise = error "SMTEN INTERNAL ERROR: unexpected arg to appEH"

errorEH :: Type -> String -> ExpH
errorEH t s = exph $ ErrorEH t s
    
exph :: ExpH_Value -> ExpH
exph v =
  let {-# NOINLINE idstore #-}
      idstore :: IORef Integer
      idstore = unsafeDupablePerformIO (newIORef 0)
  in unsafeDupablePerformIO $ do
        x <- readIORef idstore
        writeIORef idstore $! x + 1
        return $ ExpH (EID x) v

lamEH :: Sig -> Type -> (ExpH -> ExpH) -> ExpH
lamEH s t f = exph $ LamEH s t f

aconEH :: Name -> Type -> [ExpH] -> ExpH
aconEH n t args = exph $ ConEH n t args

ifEH :: Type -> ExpH -> ExpH -> ExpH -> ExpH
ifEH t p a b =
  case force p of
    ConEH k _ []
      | k == trueN -> a
      | k == falseN -> b
    ErrorEH _ s -> errorEH t s
    _ -> exph $ IfEH t p a b

caseEH :: Type -> ExpH -> Sig -> ExpH -> ExpH -> ExpH
caseEH t x k@(Sig nk _) y n
 | nk == trueN = ifEH t x y n
 | nk == falseN = ifEH t x n y
 | otherwise =
  case force x of
    ConEH k _ vs -> if k == nk then appsEH y vs else n
    ErrorEH _ s -> errorEH t s
    _ -> error $ "SMTEN INTERNAL ERROR: unexpected arg to caseEH"


appsEH :: ExpH -> [ExpH] -> ExpH
appsEH f xs = foldl appEH f xs

lamHF :: (ExpH -> ExpH) -> ExpH
lamHF f =
  let s = error "lam sig"
      bt = error "lam bt"
  in lamEH s bt f

conHF :: String -> [ExpH] -> ExpH
conHF n xs = aconEH (name n) undefined xs

caseHF :: String -> ExpH -> ExpH -> ExpH -> ExpH
caseHF nm x y n =
 let t = error "caseHF t"
     tcs = error "caseHF tcs"
     s = Sig (name nm) t
 in caseEH tcs x s y n

eq :: ExpH
eq = lamHF $ \a -> lamHF $ \b ->
        case (de_integerEH a, de_integerEH b) of
            (Just av, Just bv) -> boolEH (av == bv)

sub :: ExpH
sub = lamHF $ \a -> lamHF $ \b ->
        case (de_integerEH a, de_integerEH b) of
            (Just av, Just bv) -> integerEH (av - bv)

mklist :: ExpH
mklist = lamHF $ \i ->
            lamHF $ \v ->
               ifEH undefined (appEH (appEH eq (integerEH 0)) i)
                          (conHF "Prelude.[]" [])
                          (conHF "Prelude.:" [v, appEH (appEH mklist (appEH (appEH sub i) (integerEH 1))) v]) 
                      
myand :: ExpH
myand = lamHF $ \l ->
            caseHF "Prelude.:" l (lamHF $ \x ->
                lamHF $ \xs ->
                   ifEH undefined x (appEH myand xs) falseEH)
                trueEH
                                         
elems :: ExpH
elems = appEH (appEH mklist (integerEH 1000000)) trueEH

result :: ExpH
result = appEH myand elems

main :: IO ()
main = do
    putStrLn $ show result

