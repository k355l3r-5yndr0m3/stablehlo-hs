{-# LANGUAGE ForeignFunctionInterface, ViewPatterns #-}
module Stablehlo.Dialect.Stablehlo.Attributes (
  gatherDimensionNumbersAttr
, GatherDimensionNumbersAttr
, stablehloDotDimensionNumbersAttr
, DotDimensionNumbersAttr
, scatterDimensionNumbersAttr
, ScatterDimensionNumbersAttr
, stablehloConvDimensionNumbersAttr
, ConvDimensionNumbersAttr
, outputOperandAliasAttr
, OutputOperandAliasAttr
, channelHandleAttr
, ChannelHandleAttr
, typeExtensionsAttr
, TypeExtensionsAttr

, Transpose(..)
, TransposeAttr
, transposeAttr

, RngDistribution(..)
, RngDistributionAttr
, rngDistributionAttr

, RngAlgorithm(..)
, RngAlgorithmAttr
, rngAlgorithmAttr

, FftType(..)
, FftTypeAttr
, fftTypeAttr

, ComparisonType(..)
, ComparisonTypeAttr
, comparisonTypeAttr

, ComparisonDirection(..)
, ComparisonDirectionAttr
, comparisonDirectionAttr

, CustomCallApiVersion(..)
, CustomCallApiVersionAttr
, customCallApiVersionAttr
) where

import qualified MLIR.C.IR as C
import MLIR.IR

import Foreign
import Foreign.C


{-
MLIR_CAPI_EXPORTED MlirAttribute stablehloGatherDimensionNumbers(
    MlirContext ctx, intptr_t nOffsetDims, const int64_t *offsetDims,
    intptr_t nCollapsedSliceDims, const int64_t *collapsedSliceDims,
    intptr_t nStartIndexMap, const int64_t *startIndexMap,
    int64_t indexVectorDim);
 -}

foreign import ccall unsafe "stablehloGatherDimensionNumbersGet"
  stablehloGatherDimensionNumbersGet' :: C.Context -> CIntPtr -> Ptr Int64 -> CIntPtr -> Ptr Int64 -> CIntPtr -> Ptr Int64 -> Int64 -> IO C.Attribute
type GatherDimensionNumbersAttr = Attribute
gatherDimensionNumbersAttr :: [Int64] -> [Int64] -> [Int64] -> Int64 -> GatherDimensionNumbersAttr
gatherDimensionNumbersAttr offsetDims collapsedSliceDims startIndexMap indexVectorDim = Attribute $ \ctx ->
  withArrayLen offsetDims $ \(fromIntegral -> nOffsetDims) offsetDims' -> 
    withArrayLen collapsedSliceDims $ \(fromIntegral -> nCollapsedSliceDims) collapsedSliceDims' ->
      withArrayLen startIndexMap $ \(fromIntegral -> nStartIndexMap) startIndexMap' ->
        stablehloGatherDimensionNumbersGet' ctx nOffsetDims offsetDims' nCollapsedSliceDims collapsedSliceDims' nStartIndexMap startIndexMap' indexVectorDim

{-
MLIR_CAPI_EXPORTED MlirAttribute stablehloScatterDimensionNumbers(
    MlirContext ctx,                                                  //
    intptr_t nUpdateWindowDims, const int64_t *updateWindowDims,      //
    intptr_t nInsertedWindowDims, const int64_t *insertedWindowDims,  //
    intptr_t nScatteredDimsToOperandDims,                             //
    const int64_t *scatteredDimsToOperandDims,                        //
    int64_t indexVectorDim);
-}
foreign import ccall unsafe "stablehloScatterDimensionNumbersGet"
  stablehloScatterDimensionNumbersGet' :: C.Context -> CIntPtr -> Ptr Int64 -> CIntPtr -> Ptr Int64 -> CIntPtr -> Ptr Int64 -> Int64 -> IO C.Attribute
type ScatterDimensionNumbersAttr = Attribute
scatterDimensionNumbersAttr :: [Int64] -> [Int64] -> [Int64] -> Int64 -> ScatterDimensionNumbersAttr
scatterDimensionNumbersAttr updateWindowDims insertWindowDims scatteredDimToOperandDims indexVectorDim = Attribute $ \ctx ->
  withArrayLen updateWindowDims $ \(fromIntegral -> nUpdateWindowDims) updateWindowDims' ->
    withArrayLen insertWindowDims $ \(fromIntegral -> nInsertWindowDims) insertWindowDims' ->
      withArrayLen scatteredDimToOperandDims $ \(fromIntegral -> nScatteredDimToOperandDims) scatteredDimToOperandDims' ->
        stablehloScatterDimensionNumbersGet' ctx 
                                             nUpdateWindowDims updateWindowDims'
                                             nInsertWindowDims insertWindowDims'
                                             nScatteredDimToOperandDims scatteredDimToOperandDims'
                                             indexVectorDim
                                             



{-
MLIR_CAPI_EXPORTED MlirAttribute stablehloDotDimensionNumbers(
    MlirContext ctx,                                                        //
    intptr_t nLhsBatchingDimensions, const int64_t *lhsBatchingDimensions,  //
    intptr_t nRhsBatchingDimensions, const int64_t *rhsBatchingDimensions,  //
    intptr_t nLhsContractingDimensions,                                     //
    const int64_t *lhsContractingDimensions,                                //
    intptr_t nRhsContractingDimensions,                                     //
    const int64_t *rhsContractingDimensions);
 -}
foreign import ccall unsafe "stablehloDotDimensionNumbersGet"
  stablehloDotDimensionNumbersGet' :: C.Context -> CIntPtr -> Ptr Int64 -> CIntPtr -> Ptr Int64 -> CIntPtr -> Ptr Int64 -> CIntPtr -> Ptr Int64 -> IO C.Attribute
type DotDimensionNumbersAttr = Attribute
stablehloDotDimensionNumbersAttr :: [Int64] -> [Int64] -> [Int64] -> [Int64] -> DotDimensionNumbersAttr
stablehloDotDimensionNumbersAttr lhsBatchingDims rhsBatchingDims lhsContractingDims rhsContractingDims = Attribute $ \ctx -> 
  withArrayLen lhsBatchingDims $ \(fromIntegral -> nLhsBatchingDims) lhsBatchingDims' -> 
    withArrayLen rhsBatchingDims $ \(fromIntegral -> nRhsBatchingDims) rhsBatchingDims' -> 
      withArrayLen lhsContractingDims $ \(fromIntegral -> nLhsContractingDims) lhsContractingDims' ->
        withArrayLen rhsContractingDims $ \(fromIntegral -> nRhsContractingDims) rhsContractingDims' -> 
          stablehloDotDimensionNumbersGet' ctx
                                           nLhsBatchingDims    lhsBatchingDims'
                                           nRhsBatchingDims    rhsBatchingDims'
                                           nLhsContractingDims lhsContractingDims'
                                           nRhsContractingDims rhsContractingDims'


{-
MLIR_CAPI_EXPORTED MlirAttribute stablehloConvDimensionNumbers(
    MlirContext ctx, int64_t inputBatchDimension, int64_t inputFeatureDimension,
    intptr_t nInputSpatialDimensions, const int64_t *inputSpatialDimensions,
    int64_t kernelInputFeatureDimension, int64_t kernelOutputFeatureDimension,
    intptr_t nKernelSpatialDimensions, const int64_t *kernelSpatialDimensions,
    int64_t outputBatchDimension, int64_t outputFeatureDimension,
    intptr_t nOutputSpatialDimensions, const int64_t *outputSpatialDimensions);
-}
foreign import ccall unsafe "stablehloConvDimensionNumbersGet"
  stablehloConvDimensionNumbersGet' :: C.Context -> Int64 -> Int64 -> CIntPtr -> Ptr Int64 -> Int64 -> Int64 -> CIntPtr -> Ptr Int64 -> Int64 -> Int64 -> CIntPtr -> Ptr Int64 -> IO C.Attribute
type ConvDimensionNumbersAttr = Attribute
stablehloConvDimensionNumbersAttr :: Int64 -> Int64 -> [Int64] -> Int64 -> Int64 -> [Int64] -> Int64 -> Int64 -> [Int64] -> ConvDimensionNumbersAttr
stablehloConvDimensionNumbersAttr inputBatchingDim inputFeatureDim inputSpatialDims kernelInputFeatureDim kernelOutputFeatureDim kernelSpatialDims outputBatchingDim outputFeatureDim outputSpatialDims = 
  Attribute $ \ctx -> 
    withArrayLen inputSpatialDims $ \(fromIntegral -> nInputSpatialDims) inputSpatialDims' -> 
      withArrayLen kernelSpatialDims $ \(fromIntegral -> nKernelSpatialDims) kernelSpatialDims' -> 
        withArrayLen outputSpatialDims $ \(fromIntegral -> nOutputSpatialDims) outputSpatialDims' -> 
          stablehloConvDimensionNumbersGet' ctx inputBatchingDim      inputFeatureDim 
                                                nInputSpatialDims     inputSpatialDims' 
                                                kernelInputFeatureDim kernelOutputFeatureDim 
                                                nKernelSpatialDims    kernelSpatialDims' 
                                                outputBatchingDim     outputFeatureDim 
                                                nOutputSpatialDims    outputSpatialDims'


{-
MLIR_CAPI_EXPORTED MlirAttribute stablehloOutputOperandAlias(
    MlirContext ctx, intptr_t nOutputTupleIndices,
    const int64_t *outputTupleIndices, int64_t operandIndex,
    intptr_t nOperandTupleIndices, const int64_t *operandTupleIndices);
-}
foreign import ccall unsafe "stablehloOutputOperandAliasGet"
  stablehloOutputOperandAliasGet' :: C.Context -> CIntPtr -> Ptr Int64 -> Int64 -> CIntPtr -> Ptr Int64 -> IO C.Attribute
type OutputOperandAliasAttr = Attribute
outputOperandAliasAttr :: [Int64] -> Int64 -> [Int64] -> OutputOperandAliasAttr
outputOperandAliasAttr outputTupleIndices operandIndex operandTupleIndices = Attribute $ \ctx ->
  withArrayLen outputTupleIndices $ \(fromIntegral -> nOutputTupleIndices) outputTupleIndices' ->
    withArrayLen operandTupleIndices $ \(fromIntegral -> nOperandTupleIndices) operandTupleIndices' ->
      stablehloOutputOperandAliasGet' ctx nOutputTupleIndices outputTupleIndices' operandIndex nOperandTupleIndices operandTupleIndices'



foreign import ccall unsafe "hs__stablehloCustomCallApiVersionAttr__API_VERSION_STATUS_RETURNING_UNIFIED"
  hs__stablehloCustomCallApiVersionAttr__API_VERSION_STATUS_RETURNING_UNIFIED :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloCustomCallApiVersionAttr__API_VERSION_STATUS_RETURNING"
  hs__stablehloCustomCallApiVersionAttr__API_VERSION_STATUS_RETURNING :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloCustomCallApiVersionAttr__API_VERSION_UNSPECIFIED"
  hs__stablehloCustomCallApiVersionAttr__API_VERSION_UNSPECIFIED :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloCustomCallApiVersionAttr__API_VERSION_ORIGINAL"
  hs__stablehloCustomCallApiVersionAttr__API_VERSION_ORIGINAL :: C.Context -> IO C.Attribute
data CustomCallApiVersion = StatusReturningUnified
                          | StatusReturning
                          | Unspecified
                          | Original
type CustomCallApiVersionAttr = Attribute
customCallApiVersionAttr :: CustomCallApiVersion -> CustomCallApiVersionAttr
customCallApiVersionAttr av = Attribute $
  case av of 
    StatusReturningUnified -> hs__stablehloCustomCallApiVersionAttr__API_VERSION_STATUS_RETURNING_UNIFIED
    StatusReturning        -> hs__stablehloCustomCallApiVersionAttr__API_VERSION_STATUS_RETURNING
    Unspecified            -> hs__stablehloCustomCallApiVersionAttr__API_VERSION_UNSPECIFIED
    Original               -> hs__stablehloCustomCallApiVersionAttr__API_VERSION_ORIGINAL



foreign import ccall unsafe "hs__stablehloTransposeAttr__NO_TRANSPOSE"
  hs__stablehloTransposeAttr__NO_TRANSPOSE :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloTransposeAttr__TRANSPOSE"
  hs__stablehloTransposeAttr__TRANSPOSE :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloTransposeAttr__ADJOINT"
  hs__stablehloTransposeAttr__ADJOINT :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloTransposeAttr__TRANSPOSE_INVALID"
  hs__stablehloTransposeAttr__TRANSPOSE_INVALID :: C.Context -> IO C.Attribute
data Transpose = NoTranspose
               | Transpose
               | Adjoint
               | TransposeInvalid
type TransposeAttr = Attribute
transposeAttr :: Transpose -> TransposeAttr
transposeAttr t = Attribute $
  case t of
    NoTranspose      -> hs__stablehloTransposeAttr__NO_TRANSPOSE
    Transpose        -> hs__stablehloTransposeAttr__TRANSPOSE
    Adjoint          -> hs__stablehloTransposeAttr__ADJOINT
    TransposeInvalid -> hs__stablehloTransposeAttr__TRANSPOSE_INVALID




foreign import ccall unsafe "hs__stablehloRngDistributionAttr__NORMAL"
  hs__stablehloRngDistributionAttr__NORMAL :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloRngDistributionAttr__UNIFORM"
  hs__stablehloRngDistributionAttr__UNIFORM :: C.Context -> IO C.Attribute
data RngDistribution = Normal 
                     | Uniform
type RngDistributionAttr = Attribute
rngDistributionAttr :: RngDistribution -> RngDistributionAttr
rngDistributionAttr d = Attribute $
  case d of
    Normal  -> hs__stablehloRngDistributionAttr__NORMAL
    Uniform -> hs__stablehloRngDistributionAttr__UNIFORM





foreign import ccall unsafe "hs__stablehloRngAlgorithmAttr__THREE_FRY"
  hs__stablehloRngAlgorithmAttr__THREE_FRY :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloRngAlgorithmAttr__DEFAULT"
  hs__stablehloRngAlgorithmAttr__DEFAULT :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloRngAlgorithmAttr__PHILOX"
  hs__stablehloRngAlgorithmAttr__PHILOX :: C.Context -> IO C.Attribute
data RngAlgorithm = ThreeFry
                  | Default
                  | Philox
type RngAlgorithmAttr = Attribute
rngAlgorithmAttr :: RngAlgorithm -> RngAlgorithmAttr
rngAlgorithmAttr a = Attribute $
  case a of
    ThreeFry -> hs__stablehloRngAlgorithmAttr__THREE_FRY
    Default  -> hs__stablehloRngAlgorithmAttr__DEFAULT
    Philox   -> hs__stablehloRngAlgorithmAttr__PHILOX


foreign import ccall unsafe "hs__stablehloFftTypeAttr__IRFFT"
  hs__stablehloFftTypeAttr__IRFFT :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloFftTypeAttr__FFT"
  hs__stablehloFftTypeAttr__FFT :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloFftTypeAttr__IFFT"
  hs__stablehloFftTypeAttr__IFFT :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloFftTypeAttr__RFFT"
  hs__stablehloFftTypeAttr__RFFT :: C.Context -> IO C.Attribute
data FftType = Irfft
             | Fft
             | Ifft
             | Rfft
type FftTypeAttr = Attribute
fftTypeAttr :: FftType -> FftTypeAttr
fftTypeAttr f = Attribute $
  case f of 
    Irfft -> hs__stablehloFftTypeAttr__IRFFT
    Fft   -> hs__stablehloFftTypeAttr__FFT
    Ifft  -> hs__stablehloFftTypeAttr__IFFT
    Rfft  -> hs__stablehloFftTypeAttr__RFFT




foreign import ccall unsafe "hs__stablehloComparisonTypeAttr__FLOAT"
  hs__stablehloComparisonTypeAttr__FLOAT :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloComparisonTypeAttr__TOTALORDER"
  hs__stablehloComparisonTypeAttr__TOTALORDER :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloComparisonTypeAttr__SIGNED"
  hs__stablehloComparisonTypeAttr__SIGNED :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloComparisonTypeAttr__UNSIGNED"
  hs__stablehloComparisonTypeAttr__UNSIGNED :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloComparisonTypeAttr__NOTYPE"
  hs__stablehloComparisonTypeAttr__NOTYPE :: C.Context -> IO C.Attribute
data ComparisonType = Float
                     | TotalOrder
                     | Signed
                     | Unsigned
                     | NoType
type ComparisonTypeAttr = Attribute          
comparisonTypeAttr :: ComparisonType -> ComparisonTypeAttr 
comparisonTypeAttr c = Attribute $ 
  case c of
    Float      -> hs__stablehloComparisonTypeAttr__FLOAT
    TotalOrder -> hs__stablehloComparisonTypeAttr__TOTALORDER
    Signed     -> hs__stablehloComparisonTypeAttr__SIGNED
    Unsigned   -> hs__stablehloComparisonTypeAttr__UNSIGNED
    NoType     -> hs__stablehloComparisonTypeAttr__NOTYPE

foreign import ccall unsafe "hs__stablehloComparisonDirectionAttr__EQ"
  hs__stablehloComparisonDirectionAttr__EQ :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloComparisonDirectionAttr__LE"
  hs__stablehloComparisonDirectionAttr__LE :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloComparisonDirectionAttr__LT"
  hs__stablehloComparisonDirectionAttr__LT :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloComparisonDirectionAttr__GE"
  hs__stablehloComparisonDirectionAttr__GE :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloComparisonDirectionAttr__GT"
  hs__stablehloComparisonDirectionAttr__GT :: C.Context -> IO C.Attribute
foreign import ccall unsafe "hs__stablehloComparisonDirectionAttr__NE"
  hs__stablehloComparisonDirectionAttr__NE :: C.Context -> IO C.Attribute
data ComparisonDirection = Eq | Ne | Le | Lt | Ge | Gt -- TODO: Maybe consider using the Ordering datatype builtin to Prelude
type ComparisonDirectionAttr = Attribute
comparisonDirectionAttr :: ComparisonDirection -> ComparisonDirectionAttr
comparisonDirectionAttr c = Attribute $ 
  case c of 
    Eq -> hs__stablehloComparisonDirectionAttr__EQ
    Ne -> hs__stablehloComparisonDirectionAttr__NE
    Le -> hs__stablehloComparisonDirectionAttr__LE
    Lt -> hs__stablehloComparisonDirectionAttr__LT
    Ge -> hs__stablehloComparisonDirectionAttr__GE
    Gt -> hs__stablehloComparisonDirectionAttr__GT


{-
MLIR_CAPI_EXPORTED MlirAttribute stablehloChannelHandle(MlirContext ctx,
                                                           int64_t handle,
                                                           int64_t type);
-}
foreign import ccall unsafe "stablehloChannelHandleGet"
  stablehloChannelHandleGet' :: C.Context -> Int64 -> Int64 -> IO C.Attribute
type ChannelHandleAttr = Attribute
channelHandleAttr :: Int64 -> Int64 -> ChannelHandleAttr
channelHandleAttr handle _type = Attribute $ \ctx ->
  stablehloChannelHandleGet' ctx handle _type



{-
MLIR_CAPI_EXPORTED MlirAttribute stablehloTypeExtensions(
   MlirContext ctx, intptr_t nBounds, const int64_t *bounds);
 -}
foreign import ccall unsafe "stablehloTypeExtensionsGet"
  stablehloTypeExtensionsGet' :: C.Context -> CIntPtr -> Ptr Int64 -> IO C.Attribute
type TypeExtensionsAttr = Attribute
typeExtensionsAttr :: [Int64] -> TypeExtensionsAttr
typeExtensionsAttr bounds = Attribute $ \ctx -> withArrayLen bounds $ \(fromIntegral -> nBounds) bounds' -> stablehloTypeExtensionsGet' ctx nBounds bounds'





