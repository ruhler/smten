
module Smten.Runtime.Debug (
    Debug, dbgRender,
    dbgOp, dbgCase, dbgText, dbgError, dbgVar, dbgCon, dbgApp, dbgApps,
    dbgShare, dbgLit,
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

data MyDoc = MyDoc {
    md_doc :: Doc,
    md_share :: Bool
}

mydoc :: Doc -> MyDoc
mydoc d = MyDoc d True

noshare :: Debug -> Debug
noshare d = do
  v <- d
  return (v { md_share = False })

type DebugM = ReaderT DR IO
type Debug = DebugM MyDoc

dbgShare :: (a -> Debug) -> a -> Debug
dbgShare f x = do
  m <- ask
  fnd <- liftIO $ A.lookup (dr_seen m) x
  case fnd of
    Just v -> return $ MyDoc (text "@" <> integer v) False
    Nothing -> do
      v <- f x
      if (md_share v)
        then do
          id <- liftIO $ readIORef (dr_nid m)
          liftIO $ do
            modifyIORef' (dr_nid m) (+ 1)
            A.insert (dr_seen m) x id
          return (MyDoc (text ("@" ++ show id ++ "{") <+> (md_doc v) <+> text "}") False)
        else do
          return v

(<+>.) :: Debug -> Debug -> Debug
(<+>.) a b = do
    a' <- md_doc <$> a
    b' <- md_doc <$> b
    return $ mydoc (a' <+> b')

($+$.) :: Debug -> Debug -> Debug
($+$.) a b = do
    a' <- md_doc <$> a
    b' <- md_doc <$> b
    return $ mydoc (a' $+$ b')

text' :: String -> Debug 
text' = return . mydoc . text

sep' :: [Debug] -> Debug
sep' ms = do
  vs <- sequence ms
  return $ mydoc (sep (map md_doc vs))

vcat' :: [Debug] -> Debug
vcat' ms = do
  vs <- sequence ms
  return $ mydoc (vcat (map md_doc vs))

nest' :: Int -> Debug -> Debug
nest' i x = do 
  v <- md_doc <$> x
  return $ mydoc (nest i v)

dbgOp :: String -> Debug -> Debug -> Debug
dbgOp op a b = a <+>. text' op <+>. b

dbgApp :: Debug -> Debug -> Debug
dbgApp a b = a <+>. b

dbgApps :: Debug -> [Debug] -> Debug
dbgApps x xs = sep' (x:xs)

dbgVar :: String -> Debug
dbgVar = noshare . text'

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
dbgText = noshare . text'

dbgError :: String -> Debug
dbgError s = text' "error" <+>. text' (show s)

dbgCon :: String -> [Debug] -> Debug
dbgCon k [] = dbgText k
dbgCon k xs = sep' ((text' k) : xs)

dbgRender :: Debug -> IO String
dbgRender d = {-# SCC "DebugRender" #-} do
  m <- A.new
  id <- newIORef 0
  render . md_doc <$> runReaderT d (DR m id)

dbgLit :: (Show a) => a -> Debug
dbgLit = dbgText . show

