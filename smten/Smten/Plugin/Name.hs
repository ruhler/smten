
module Smten.Plugin.Name (
    nameCG, qnameCG,
    itenmCG, qitenmCG,
    tynameCG, qtynameCG,
    guardnmCG, qguardnmCG,
    fieldnmCG, qfieldnmCG,
    nullnmCG, qnullnmCG,
    connmCG, qconnmCG,
    denewtynmCG, qdenewtynmCG,
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

-- Given a base name, turn it into an acceptable haskell name.
-- Returns 'True' if the resulting name is symbolic, false otherwise.
--
-- If 'nosym' is True, then never return a symbolic name.
resym :: Bool -> String -> (Bool, String)
resym nosym nm =
  let issym :: Char -> Bool
      issym c = c `elem` "!#$%&*+./<=>?@\\^|-~:"

      desym :: Char -> Char
      desym c | isAlphaNum c = c
      desym c | c == '#' = c
      desym c | c == '_' = c
      desym c | c == ':' = toEnum $ fromEnum 'A' + (fromEnum c `mod` 26)
      desym c = toEnum $ fromEnum 'a' + (fromEnum c `mod` 26)
  in case (nosym, nm) of
        (False, _) | all issym nm -> (True, nm)
        _ -> (False, map desym nm)

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

          useuniq = (not $ isExternalName nm)
              || (occnm == "main" && modnm /= Just ":Main")

          isconsym = (head occnm == ':')

          (issym, occnm') = resym (useuniq || isconsym) occnm

          unqlf = f $ if useuniq
                        then occnm' ++ "_" ++ unqnm
                        else occnm'

      full <- case (qlf, modnm) of
                 (True, Just ":Main") -> return unqlf
                 (True, Just v) -> usequalified ("Smten.Compiled." ++ v) unqlf
                 _ -> return unqlf

      return $ if issym then "(" ++ full ++ ")" else full

-- Generate code for an unqualified name.
nameCG :: Name -> CG S.Name
nameCG = nmCG False id False

-- Generate code for a qualified name.
qnameCG :: Name -> CG S.Name
qnameCG = nmCG False id True

tynameCG :: Name -> CG S.Name
tynameCG = nmCG True id False

qtynameCG :: Name -> CG S.Name
qtynameCG = nmCG True id True

guardnmCG :: Name -> CG S.Name
guardnmCG = nmCG False ("gd" ++) False

qguardnmCG :: Name -> CG S.Name
qguardnmCG = nmCG False ("gd" ++) True

fieldnmCG :: Int -> Name -> CG S.Name
fieldnmCG i = nmCG False (("fl" ++) . (++ show i)) False

qfieldnmCG :: Int -> Name -> CG S.Name
qfieldnmCG i = nmCG False (("fl" ++) . (++ show i)) True

nullnmCG :: Name -> CG S.Name
nullnmCG = nmCG True ("__Null" ++) False

qnullnmCG :: Name -> CG S.Name
qnullnmCG = nmCG True ("__Null" ++) True

-- Name of the constructor function for a given constructor.
connmCG :: Name -> CG S.Name
connmCG = nmCG False ("__" ++) False

qconnmCG :: Name -> CG S.Name
qconnmCG = nmCG False ("__" ++) True

denewtynmCG :: Name -> CG S.Name
denewtynmCG = nmCG True ("__deNewTy" ++) False

qdenewtynmCG :: Name -> CG S.Name
qdenewtynmCG = nmCG True ("__deNewTy" ++) True

qitenmCG :: Name -> CG S.Name
qitenmCG = nmCG True (++ "_Ite") True

itenmCG :: Name -> CG S.Name
itenmCG = nmCG True (++ "_Ite") False

