{-# OPTIONS_GHC -pgmPgcc -optP-E -optP-DHASKELL #-}
{-# LANGUAGE CPP #-}
module Stablehlo.Dialect.Stablehlo.Attributes (
  CustomCallApiVersionAttr(..)
, ComparisonDirectionAttr(..)
, ComparisonTypeAttr(..)
, RngAlgorithmAttr(..)
, RngDistributionAttr(..)
, TransposeAttr(..)
, FftTypeAttr(..)
, ScatterDimensionNumbersAttr(..)
, GatherDimensionNumbersAttr(..)
, DotDimensionNumbersAttr(..)
, ConvDimensionNumbersAttr(..)
, ChannelHandleAttr(..)
) where


import MLIR.C.IR
import MLIR.IR

import Foreign
import Foreign.C
import Control.Exception (assert)

#include<Dialect/Stablehlo/attributes.cpp>
data CustomCallApiVersionAttr = StatusReturningUnified | StatusReturning | Unspecified | Original
instance AttrGet CustomCallApiVersionAttr where
  attrGet StatusReturningUnified = hs__stablehloCustomCallApiVersionAttrGet__API_VERSION_STATUS_RETURNING_UNIFIED
  attrGet StatusReturning        = hs__stablehloCustomCallApiVersionAttrGet__API_VERSION_STATUS_RETURNING
  attrGet Unspecified            = hs__stablehloCustomCallApiVersionAttrGet__API_VERSION_UNSPECIFIED
  attrGet Original               = hs__stablehloCustomCallApiVersionAttrGet__API_VERSION_ORIGINAL
data ComparisonDirectionAttr = ComparisonDirectionEQ | ComparisonDirectionLE | ComparisonDirectionLT | ComparisonDirectionGE | ComparisonDirectionGT | ComparisonDirectionNE
instance AttrGet ComparisonDirectionAttr where
  attrGet ComparisonDirectionEQ = hs__stablehloComparisonDirectionAttrGet__EQ
  attrGet ComparisonDirectionLE = hs__stablehloComparisonDirectionAttrGet__LE
  attrGet ComparisonDirectionLT = hs__stablehloComparisonDirectionAttrGet__LT
  attrGet ComparisonDirectionGE = hs__stablehloComparisonDirectionAttrGet__GE
  attrGet ComparisonDirectionGT = hs__stablehloComparisonDirectionAttrGet__GT
  attrGet ComparisonDirectionNE = hs__stablehloComparisonDirectionAttrGet__NE
data ComparisonTypeAttr = ComparisonTypeFloat  | ComparisonTypeTotalOrder
                        | ComparisonTypeSigned | ComparisonTypeUnsigned
                        | ComparisonTypeNoType
instance AttrGet ComparisonTypeAttr where
  attrGet ComparisonTypeFloat      = hs__stablehloComparisonTypeAttrGet__FLOAT
  attrGet ComparisonTypeTotalOrder = hs__stablehloComparisonTypeAttrGet__TOTALORDER
  attrGet ComparisonTypeSigned     = hs__stablehloComparisonTypeAttrGet__SIGNED
  attrGet ComparisonTypeUnsigned   = hs__stablehloComparisonTypeAttrGet__UNSIGNED
  attrGet ComparisonTypeNoType     = hs__stablehloComparisonTypeAttrGet__NOTYPE
data RngAlgorithmAttr = RngAlgoThreeFry | RngAlgoDefault | RngAlgoPhilox
instance AttrGet RngAlgorithmAttr where
  attrGet RngAlgoThreeFry = hs__stablehloRngAlgorithmAttrGet__THREE_FRY
  attrGet RngAlgoDefault  = hs__stablehloRngAlgorithmAttrGet__DEFAULT
  attrGet RngAlgoPhilox   = hs__stablehloRngAlgorithmAttrGet__PHILOX
data RngDistributionAttr = RngDistNormal | RngDistUniform
instance AttrGet RngDistributionAttr where
  attrGet RngDistNormal  = hs__stablehloRngDistributionAttrGet__NORMAL
  attrGet RngDistUniform = hs__stablehloRngDistributionAttrGet__UNIFORM
data TransposeAttr = NoTranspose | Transpose | Adjoint | TransposeInvalid
instance AttrGet TransposeAttr where
  attrGet NoTranspose      = hs__stablehloTransposeAttrGet__NO_TRANSPOSE
  attrGet Transpose        = hs__stablehloTransposeAttrGet__TRANSPOSE
  attrGet Adjoint          = hs__stablehloTransposeAttrGet__ADJOINT
  attrGet TransposeInvalid = hs__stablehloTransposeAttrGet__TRANSPOSE_INVALID
data FftTypeAttr = IRFFT | FFT | IFFT | RFFT
instance AttrGet FftTypeAttr where
  attrGet IRFFT = hs__stablehloFftTypeAttrGet__IRFFT
  attrGet FFT   = hs__stablehloFftTypeAttrGet__FFT
  attrGet IFFT  = hs__stablehloFftTypeAttrGet__IFFT
  attrGet RFFT  = hs__stablehloFftTypeAttrGet__RFFT

#define LenArray(note) CIntPtr -> Ptr Int64 

data ScatterDimensionNumbersAttr
instance AttrGet ScatterDimensionNumbersAttr where
foreign import ccall unsafe "stablehloScatterDimensionNumbersGet"
  scatterDimensionNumbersGet :: Context -> LenArray(update window) -> LenArray(insert window) -> LenArray(scatter dims to operand dims) -> Int64 -> IO Attribute



data GatherDimensionNumbersAttr
instance AttrGet GatherDimensionNumbersAttr where
foreign import ccall unsafe "stablehloGatherDimensionNumbersGet"
  gatherDimensionNumbersGet :: Context -> LenArray(offset dims) -> LenArray(collapsed slice dims) -> LenArray(start index map) -> Int64 -> IO Attribute


data DotDimensionNumbersAttr = DotDimensionNumbersAttr { getBatchingDims :: [(Int64, Int64)], getContractingDims :: [(Int64, Int64)] }
instance AttrGet DotDimensionNumbersAttr where
  attrGet attr c = assert constraints $ do 
    withArray lhsBatch $ \ lhsBatch' -> 
      withArray rhsBatch $ \ rhsBatch' -> 
        withArray lhsContr $ \ lhsContr' ->
          withArray rhsContr $ \ rhsContr' -> do
            putStrLn ("numBatch is: " ++ show numBatchDims)
            dotDimensionNumbersGet c numBatchDims lhsBatch' numBatchDims rhsBatch' numContrDims lhsContr' numContrDims rhsContr'
    where (lhsBatch, rhsBatch) = unzip $ getBatchingDims attr 
          (lhsContr, rhsContr) = unzip $ getContractingDims attr
          isUnique :: Eq a => [a] -> Bool
          isUnique []    = True 
          isUnique (a:as) = (a `notElem` as) && isUnique as
          numBatchDims = fromIntegral $ length $ getBatchingDims attr 
          numContrDims = fromIntegral $ length $ getContractingDims attr 
          constraints = isUnique (lhsBatch ++ lhsContr) && isUnique (rhsBatch ++ rhsContr)
          
  
foreign import ccall unsafe "stablehloDotDimensionNumbersGet"
  dotDimensionNumbersGet :: Context -> LenArray(lhs batching) -> LenArray(rhs batching) -> LenArray(lhs contracting) -> LenArray(rhs contracting) -> IO Attribute


data ConvDimensionNumbersAttr
instance AttrGet ConvDimensionNumbersAttr where
foreign import ccall unsafe "stablehloConvDimensionNumbersGet"
  convDimensionNumbersGet :: Context -> Int64 -> Int64 -> LenArray(input spatial dims) -> Int64 -> Int64 -> LenArray(kernel spatial dims) -> Int64 -> Int64 -> LenArray(output spatial dims) -> IO Attribute


foreign import ccall unsafe "stablehloOutputOperandAliasGet"
  outputOperandAliasGet :: Context -> LenArray(output tuple indices) -> Int64 -> LenArray(operand tuple indices) -> IO Attribute

data ChannelHandleAttr -- TODO: Implement
instance AttrGet ChannelHandleAttr where


