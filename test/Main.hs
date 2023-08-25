module Main (main) where
import qualified Stablehlo.Dialect.Stablehlo as Stablehlo
import qualified MLIR.Dialect.Func as Func
import MLIR.IR
import MLIR.BuiltinAttributes (stringAttr, typeAttr, attributeNull)
import MLIR.BuiltinTypes (functionType, f32Type, rankedTensorType)

main :: IO ()
main = withContext $ do
  loadDialect_ Func.dialect
  loadDialect_ Stablehlo.dialect
  m <- moduleOp $ do 
    Func._FuncOp (stringAttr "main")
            (typeAttr $ functionType [rankedTensorType [] f32Type attributeNull, rankedTensorType [] f32Type attributeNull] [rankedTensorType [] f32Type attributeNull])
            Nothing Nothing Nothing $ do
              bb0 <- addBlock [rankedTensorType [] f32Type attributeNull, rankedTensorType [] f32Type attributeNull]
              defBlock bb0 $ do
                a0 <- blkArg 0
                a1 <- blkArg 1
                _0 <- Stablehlo._AddOp a0 a1 (rankedTensorType [] f32Type attributeNull)
                Func._ReturnOp [_0]
              return ()
    return ()
  dumpModule m
  moduleDestroy m
  return ()
