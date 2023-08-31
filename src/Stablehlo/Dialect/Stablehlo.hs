{-# LANGUAGE ForeignFunctionInterface #-}
module Stablehlo.Dialect.Stablehlo (
  module Ops
, dialect
) where
import Stablehlo.Dialect.Stablehlo.Ops as Ops
import MLIR.C.IR (DialectHandle(..))

foreign import ccall unsafe "mlirGetDialectHandle__stablehlo__" 
  dialect :: DialectHandle

