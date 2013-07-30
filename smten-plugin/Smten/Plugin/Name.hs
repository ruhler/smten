
module Smten.Plugin.Name (
    nameCG, qnameCG, qtynameCG,
    primnmCG, qprimnmCG,
    errnmCG, qerrnmCG,
    iteerrnmCG, qiteerrnmCG,
    iteflnmCG, qiteflnmCG,
    itenmCG, qitenmCG,
    nullitenmCG, qnullitenmCG,
    liftitenmCG, qliftitenmCG,
    ) where

import Data.Char
import Data.Functor

import GhcPlugins
import Smten.Plugin.CG
import qualified Smten.Plugin.Output.Syntax as S

nameis :: String -> Name -> Bool
nameis str nm = str == (occNameString $ nameOccName nm)

-- dename ty nm
-- Extract the module name, occurence name, and unique name for the given
-- name.
--   ty - true if this is a type constructor name.
--   nm - the name.
dename :: Bool -> Name -> (Maybe String, String, String)
dename ty nm
  | nameis "[]" nm = (Just "Smten.Smten.Base",
                      if ty then "List__" else "Nil__",
                      error "uniqnm for []")
  | nameis ":" nm = (Just "Smten.Smten.Base", "Cons__", error "uniqnm for :")
  | nameis "()" nm = (Just "Smten.Smten.Base", "Unit__", error "uniqnm for ()")
  | nameis "(,)" nm = (Just "Smten.Smten.Base", "Tuple2__", error "uniqnm for (,)")
  | nameis "(,,)" nm = (Just "Smten.Smten.Base", "Tuple3__", error "uniqnm for (,)")
  | nameis "(,,,)" nm = (Just "Smten.Smten.Base", "Tuple4__", error "uniqnm for (,)")
  | otherwise = 
      let modnm = moduleNameString . moduleName <$> nameModule_maybe nm
          occnm = occNameString $ nameOccName nm
          unqnm = show $ nameUnique nm
      in (modnm, occnm, unqnm)

-- nmCG ty f qlf nm
-- translate a name to a smten name.
--  ty - True if this is a type constructor name.
--  f - transformation to perform on the base of the name.
--  qlf - True to generate a qualified version of the name.
--        False to generate an unqualified version of the name.
--  nm - The name to transform.
nmCG :: Bool -> (String -> String) -> Bool -> Name -> CG S.Name
nmCG ty f qlf nm
  | nameis "(->)" nm = return "(->)"
  | otherwise = do
      let (modnm, occnm, unqnm) = dename ty nm

          desym :: Char -> Char
          desym c | isAlphaNum c = c
          desym c | c == '#' = c
          desym c | c == '_' = c
          desym c | c == ':' = toEnum $ fromEnum 'A' + (fromEnum c `mod` 26)
          desym c = toEnum $ fromEnum 'a' + (fromEnum c `mod` 26)

          occnm' = map desym occnm

          useuniq = (not $ isExternalName nm)
              || (occnm == "main" && modnm /= Just ":Main")

          unqlf = f $ if useuniq
                        then occnm' ++ "_" ++ unqnm
                        else occnm'
      if qlf
         then case modnm of
                Just v -> do addimport $ "Smten.Compiled." ++ v
                             return $ "Smten.Compiled." ++ v ++ "." ++ unqlf
                _ -> return unqlf
         else return unqlf

-- Generate code for an unqualified name.
nameCG :: Name -> CG S.Name
nameCG = nmCG False id False

-- Generate code for a qualified name.
qnameCG :: Name -> CG S.Name
qnameCG = nmCG False id True

-- Generate code for a qualified name.
qtynameCG :: Name -> CG S.Name
qtynameCG = nmCG True id True

-- | Generate code for the prim constructor of a given data type.
primnmCG :: Name -> CG S.Name
primnmCG = nmCG True (++ "_Prim") False

-- | qualified type constructor name.
qprimnmCG :: Name -> CG S.Name
qprimnmCG = nmCG True (++ "_Prim") True

-- | Generate code for the err constructor of a given data type.
errnmCG :: Name -> CG S.Name
errnmCG = nmCG True (++ "_Err") False

-- | qualified type constructor name.
qerrnmCG :: Name -> CG S.Name
qerrnmCG = nmCG True (++ "_Err") True

-- | Generate code for the ite constructor of a given data type.
itenmCG :: Name -> CG S.Name
itenmCG = nmCG True (++ "_Ite") False

-- | qualified type constructor name.
qitenmCG :: Name -> CG S.Name
qitenmCG = nmCG True (++ "_Ite") True

iteflnmCG :: Name -> CG S.Name
iteflnmCG = nmCG False ("__ite" ++) False

qiteflnmCG :: Name -> CG S.Name
qiteflnmCG = nmCG False ("__ite" ++) True

iteerrnmCG ::  Name -> CG S.Name
iteerrnmCG = nmCG True ("__iteErr" ++) False

qiteerrnmCG :: Name -> CG S.Name
qiteerrnmCG = nmCG True ("__iteErr" ++) True

nullitenmCG :: Name -> CG S.Name
nullitenmCG = nmCG True ("__NullIte" ++) False

qnullitenmCG :: Name -> CG S.Name
qnullitenmCG = nmCG True ("__NullIte" ++) True

liftitenmCG :: Name -> CG S.Name
liftitenmCG = nmCG True ("__LiftIte" ++) False

qliftitenmCG :: Name -> CG S.Name
qliftitenmCG = nmCG True ("__LiftIte" ++) True

