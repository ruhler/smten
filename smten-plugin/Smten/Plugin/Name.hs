
module Smten.Plugin.Name (
    nameCG, qnameCG,
    ) where

import Data.Char
import Data.Functor
import Data.List

import GhcPlugins
import Smten.Plugin.CG
import qualified Smten.Plugin.Output.Syntax as S

nameis :: String -> Name -> Bool
nameis str nm = str == (occNameString $ nameOccName nm)

nmCG :: Bool -> Name -> CG S.Name
nmCG q nm
  | nameis "(->)" nm = return "(->)"
  | nameis "[]" nm = return "[]"
  | nameis ":" nm = return "(:)"
  | nameis "()" nm = return "()"
  | nameis "(,)" nm = return "(,)"
  | nameis "(,,)" nm = return "(,,)"
  | nameis "(,,,)" nm = return "(,,,)"
  | otherwise = do
      isl <- islocal nm
      let modnm = moduleNameString . moduleName <$> nameModule_maybe nm
          occnm = occNameString $ nameOccName nm
          unqnm = show $ nameUnique nm

          desym :: Char -> Char
          desym c | isAlphaNum c = c
          desym c | c == '#' = c
          desym c | c == '_' = c
          desym c = toEnum $ fromEnum 'a' + (fromEnum c `mod` 26)

          occnm' = map desym occnm

          useuniq = isPrefixOf "$c" occnm
            || (isPrefixOf "$d" occnm && not (isPrefixOf "$dm" occnm))
            || isl
            || (occnm == "main" && modnm /= Just ":Main")
          unqlf = if useuniq
                    then occnm' ++ "_" ++ unqnm
                    else occnm'
      if q
         then case modnm of
                Just v -> do addimport $ "Smten.Compiled." ++ v
                             return $ "Smten.Compiled." ++ v ++ "." ++ unqlf
                _ -> return unqlf
         else return unqlf

-- Generate code for an unqualified name.
nameCG :: Name -> CG S.Name
nameCG = nmCG False

-- Generate code for a qualified name.
qnameCG :: Name -> CG S.Name
qnameCG = nmCG True

