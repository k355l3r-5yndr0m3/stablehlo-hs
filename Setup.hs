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
    let dialectOutput = fromFlag (buildDistPref buildFlags) </> "build" </> "autogen" </> "Stablehlo" </> "Dialect"
        mlirInclude   = fromFlag (buildDistPref buildFlags) </> "build" </> "include"
        shloInclude   = fromFlag (buildDistPref buildFlags) </> "build" </> "stablehlo"
    createDirectoryIfMissing True dialectOutput
    -- Stablehlo dialect
    createDirectoryIfMissing True $ dialectOutput </> "Stablehlo"
    hsGenerate [mlirInclude, shloInclude] 
               (shloInclude </> "stablehlo/dialect/StablehloOps.td")
               "Stablehlo.Dialect.Stablehlo.Ops"
               ["Stablehlo.Dialect.Stablehlo.Attributes"]
               (dialectOutput </> "Stablehlo" </> "Ops" <.> "hs")

    -- Do the other things
    preBuild baseHooks args buildFlags) }


{-
 -
    createDirectoryIfMissing True $ dialectOutput </> "DL"
    hsGenerate ["/home/cha0s/mlir-output/include"] 
               "/home/cha0s/mlir-output/include/mlir/Dialect/DL/IR/DLOps.td"
               "MLIR.Dialect.DL.Ops"
               []
               (dialectOutput </> "DL" </> "Ops" <.> "hs")
 -
 - -}






main :: IO ()
main = defaultMainWithHooks userHooks
