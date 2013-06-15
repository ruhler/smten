
module Smten.Runtime.Debug (
    Debug, dbgRender,
    dbgOp, dbgCase, dbgText, dbgError, dbgVar, dbgCon, dbgApp, dbgApps,
    dbgShare,
    ) where

import Control.Monad.Reader
import Data.Functor ((<$>))
import Data.IORef
import Text.PrettyPrint.HughesPJ
import qualified Smten.AnyKeyMap as A

data DR = DR {
    dr_seen :: A.AnyKeyMap Integer,
    dr_nid :: IORef Integer
}

type DebugM = ReaderT DR IO
type Debug = DebugM Doc

dbgShare :: (a -> Debug) -> a -> Debug
dbgShare f x = do
  m <- ask
  fnd <- liftIO $ A.lookup (dr_seen m) x
  case fnd of
    Just v -> return (text "@" <> integer v)
    Nothing -> do
      v <- f x
      id <- liftIO $ readIORef (dr_nid m)
      liftIO $ do
        modifyIORef' (dr_nid m) (+ 1)
        A.insert (dr_seen m) x id
      return $ text ("@" ++ show id ++ "{") <+> v <+> text "}"

(<+>.) :: Debug -> Debug -> Debug
(<+>.) a b = do
    a' <- a
    b' <- b
    return (a' <+> b')

($+$.) :: Debug -> Debug -> Debug
($+$.) a b = do
    a' <- a
    b' <- b
    return (a' $+$ b')

text' :: String -> Debug 
text' = return . text

sep' :: [Debug] -> Debug
sep' ms = do
  vs <- sequence ms
  return (sep vs)

vcat' :: [Debug] -> Debug
vcat' ms = do
  vs <- sequence ms
  return (vcat vs)

nest' :: Int -> Debug -> Debug
nest' i x = nest i <$> x

dbgOp :: String -> Debug -> Debug -> Debug
dbgOp op a b = a <+>. text' op <+>. b

dbgApp :: Debug -> Debug -> Debug
dbgApp a b = a <+>. b

dbgApps :: Debug -> [Debug] -> Debug
dbgApps x xs = sep' (x:xs)

dbgVar :: String -> Debug
dbgVar = text'

tabwidth :: Int
tabwidth = 2

dbgCase :: String -> Debug -> Debug -> Debug -> Debug
dbgCase k x y n
 | k == "True"
     = text' "if" <+>. x $+$.
        nest' tabwidth (vcat' [text' "then" <+>. y,
                             text' "else" <+>. n])
 | otherwise
     = text' "case" <+>. x <+>. text' "of" <+>. text' "{"
         $+$. nest' tabwidth (vcat' [
                 text' k <+>. text' "->" <+>. y,
                 text' "_" <+>. text' "->" <+>. n
              ]) $+$. text' "}"

dbgText :: String -> Debug
dbgText = text'

dbgError :: String -> Debug
dbgError s = text' "error" <+>. text' (show s)

dbgCon :: String -> [Debug] -> Debug
dbgCon k xs = sep' ((text' k) : xs)

dbgRender :: Debug -> IO String
dbgRender d = {-# SCC "DebugRender" #-} do
  m <- A.new
  id <- newIORef 0
  render <$> runReaderT d (DR m id)

