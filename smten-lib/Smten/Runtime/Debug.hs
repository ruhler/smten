
module Smten.Runtime.Debug (
    Debug, dbgRender,
    dbgOp, dbgCase, dbgText, dbgError, dbgVar, dbgCon, dbgApp, dbgApps,
    dbgShare, dbgLit,
    ) where

import Control.Monad.Reader
import Data.Functor ((<$>))
import Data.IORef
import Text.PrettyPrint.HughesPJ
import qualified Smten.Runtime.AnyKeyMap as A

data ShareState =
   SeenOnce             -- ^ We have seen the object once so far.
 | SeenMulti            -- ^ We have seen the object multiple times,
                        --   but have not yet shown it.
 | ShownMulti Integer   -- ^ We have seen the object multiple times,
                        --   and have already shown it as having the given id.

-- Reader data for determining which expressions are shared.
data SR = SR {
    -- Map of expressions seen.
    sr_seen :: A.AnyKeyMap ShareState
}

-- Reader data for the doc generation pass.
data GR = GR {
    gr_seen :: A.AnyKeyMap ShareState,
    gr_nid :: IORef Integer
}

data Debug = Debug {
    -- The sharing pass.
    -- In this pass, we determine which expressions are shared.
    -- The sr_seen map is updated during the traversal.
    -- The return Bool indicates whether or not this value is shareable.
    _db_sh :: ReaderT SR IO Bool,

    -- The doc pass.
    -- Generates the actual document, using the result of sharing analysis
    -- from the first pass to figure out which expressions to annotate.
    _db_go :: ReaderT GR IO Doc
}

dbgShare :: (a -> Debug) -> a -> Debug
dbgShare f x =
  let Debug shx gox = f x
      sh = do
          m <- ask
          fnd <- liftIO $ A.lookup (sr_seen m) x
          case fnd of
            Nothing -> do
                shareable <- shx
                if shareable
                    then liftIO $ A.insert (sr_seen m) x SeenOnce >> return False
                    else return False
            Just SeenOnce -> liftIO $ A.insert (sr_seen m) x SeenMulti >> return False
            Just SeenMulti -> return False
            Just (ShownMulti v) -> error "unexpected ShownMulti in sh pass"

      go = do
          m <- ask
          fnd <- liftIO $ A.lookup (gr_seen m) x
          case fnd of   
             Just (ShownMulti v) -> return (text "@" <> integer v)
             Just SeenMulti -> do
                d <- gox
                v <- liftIO $ readIORef (gr_nid m)
                liftIO $ do
                  modifyIORef' (gr_nid m) (+ 1)
                  A.insert (gr_seen m) x (ShownMulti v)
                  return $ text ("@" ++ show v ++ "{") <+> d <+> text "}"
             _ -> gox
  in Debug sh go

b :: (Doc -> Doc -> Doc) -> Debug -> Debug -> Debug
b f (Debug sha goa) (Debug shb gob) =
  let sh = sha >> shb >> return True 
      go = do
        da <- goa
        db <- gob
        return $ f da db
  in Debug sh go

(<+>.) :: Debug -> Debug -> Debug
(<+>.) = b (<+>)

($+$.) :: Debug -> Debug -> Debug
($+$.) = b ($+$)

text' :: String -> Debug 
text' s = Debug (return True) (return $ text s)

l :: ([Doc] -> Doc) -> [Debug] -> Debug
l f xs =
  let sh = sequence [s | Debug s _ <- xs] >> return True 
      go = do
        ds <- sequence [g | Debug _ g <- xs]
        return (f ds)
  in Debug sh go
  

sep' :: [Debug] -> Debug
sep' = l sep

vcat' :: [Debug] -> Debug
vcat' = l vcat

nest' :: Int -> Debug -> Debug
nest' i (Debug shx gox) = Debug shx (nest i <$> gox)

dbgOp :: String -> Debug -> Debug -> Debug
dbgOp op a b = a <+>. text' op <+>. b

dbgApp :: Debug -> Debug -> Debug
dbgApp a b = a <+>. b

dbgApps :: Debug -> [Debug] -> Debug
dbgApps x xs = sep' (x:xs)

dbgVar :: String -> Debug
dbgVar = dbgText

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

-- Raw text is considered not-shareable.
dbgText :: String -> Debug
dbgText s = Debug (return False) (return $ text s)

dbgError :: String -> Debug
dbgError s = text' "error" <+>. text' (show s)

dbgCon :: String -> [Debug] -> Debug
dbgCon k [] = dbgText k
dbgCon k xs = sep' ((text' k) : xs)

dbgLit :: (Show a) => a -> Debug
dbgLit = dbgText . show

dbgRender :: Debug -> IO String
dbgRender (Debug sh go) = {-# SCC "DebugRender" #-} do
  m <- A.new
  v <- newIORef 0
  _ <- runReaderT sh (SR m)
  render <$> runReaderT go (GR m v)

