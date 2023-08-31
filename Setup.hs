import Distribution.Simple
import Distribution.Simple.Setup
import System.Directory
import System.FilePath
import MLIR.TblGen

baseHooks :: UserHooks
baseHooks = autoconfUserHooks

userHooks :: UserHooks
userHooks = baseHooks { 
  preBuild = (\args buildFlags -> do
    generateModules (fromFlag (buildDistPref buildFlags))
    preBuild baseHooks args buildFlags),
  preRepl  = (\args replFlags -> do 
    generateModules (fromFlag (replDistPref replFlags))
    preRepl baseHooks args replFlags) }


generateModules :: FilePath -> IO ()
generateModules distPref = do 
  let dialectOutput = distPref </> "build" </> "autogen" </> "Stablehlo" </> "Dialect"
      mlirInclude   = distPref </> "build" </> "include"
      shloInclude   = distPref </> "build" </> "stablehlo"
  createDirectoryIfMissing True dialectOutput
  -- Stablehlo dialect
  createDirectoryIfMissing True $ dialectOutput </> "Stablehlo"
  hsGenerate [mlirInclude, shloInclude] 
             (shloInclude </> "stablehlo/dialect/StablehloOps.td")
             "stablehlo-typemap.h"
             "Stablehlo.Dialect.Stablehlo.Ops"
             ["Stablehlo.Dialect.Stablehlo.Attributes"]
             (dialectOutput </> "Stablehlo" </> "Ops" <.> "hs")

main :: IO ()
main = defaultMainWithHooks userHooks
