
module Smten.Plugin.Plugin (plugin) where

import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
 }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  reinitializeGlobals
  return (CoreDoPluginPass "Smten" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass m = do
  let tycons = mg_tcs m
  mapM_ dotycon tycons
  putMsg (ppr (mg_binds m))
  return m

dotycon :: TyCon -> CoreM ()
dotycon t = do
  putMsg (ppr (tyConName t))
  putMsg (ppr (tyConTyVars t))
  mapM_ dodatacon (tyConDataCons t)

dodatacon :: DataCon -> CoreM ()
dodatacon t = do
  putMsg (ppr (dataConName t))
  putMsg (ppr (dataConOrigArgTys t))

