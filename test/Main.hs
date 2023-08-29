{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import MLIR

import qualified Stablehlo.Dialect.Stablehlo as Stablehlo
import qualified MLIR.Dialect.Func as Func


main :: IO ()
main = runContextM $ do
  loadDialect_ Func.dialect
  loadDialect_ Stablehlo.dialect
  m <- moduleOp $ do 
    Func._FuncOp "main"
            (TypeAttr $ FunctionType [toAnyType $ RankedTensorType [12, 12] F32Type NullAttr, toAnyType $ RankedTensorType [12, 12] F32Type NullAttr] [toAnyType $ RankedTensorType [12, 12] F32Type NullAttr])
            Nothing Nothing Nothing $ do
              bb0 <- blockGet [toAnyType $ RankedTensorType [12, 12] F32Type NullAttr, toAnyType $ RankedTensorType [12, 12] F32Type NullAttr]
              blockDef bb0 $ do
                a0 <- blockArg 0
                a1 <- blockArg 1
                _0 <- Stablehlo._AddOp a0 a1 (toAnyType $ RankedTensorType [12, 12] F32Type NullAttr)
                Func._ReturnOp [_0]
              return ()
    return ()
  moduleDump m
  moduleDestroy m
  return ()
