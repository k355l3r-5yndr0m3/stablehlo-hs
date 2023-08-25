#include <mlir-c/IR.h>
#include <mlir/CAPI/IR.h>
#include <stablehlo/dialect/StablehloOps.h>
#include <cstdint>

#define HS_ENUM_VALUE(type, hname, cname, value) extern "C" type hs__ ## hname ## __ ## value() { return (type) cname::value; }
#define HS_ENUM_ATTR(hname, cname, value) extern "C" MlirAttribute hs__ ## hname ## __ ## value(MlirContext context) { return wrap(cname ## Attr::get(unwrap(context), cname::value)); }

// static_assert(sizeof(mlir::stablehlo::CustomCallApiVersion) == sizeof(uint32_t), "mlir::stablehlo::CustomCallApiVersion bitwidth changed.");
// #define HS_ENUM_STABLEHLO_CUSTOMCALLAPIVERSION(value) HS_ENUM_VALUE(uint32_t, stablehloCustomCallApiVersion, mlir::stablehlo::CustomCallApiVersion, value)

// HS_ENUM_STABLEHLO_CUSTOMCALLAPIVERSION(API_VERSION_ORIGINAL)
// HS_ENUM_STABLEHLO_CUSTOMCALLAPIVERSION(API_VERSION_STATUS_RETURNING_UNIFIED)
// HS_ENUM_STABLEHLO_CUSTOMCALLAPIVERSION(API_VERSION_STATUS_RETURNING)
// HS_ENUM_STABLEHLO_CUSTOMCALLAPIVERSION(API_VERSION_UNSPECIFIED)
// MlirAttribute hs__stablehloeCustomCallApiVersionGet(MlirContext context, uint32_t val) { return wrap(mlir::stablehlo::CustomCallApiVersionAttr::get(unwrap(context), (mlir::stablehlo::CustomCallApiVersion)val)); }
#define HS_ENUM_STABLEHLO_CUSTOMECALLAPIVERSIONATTR(value) HS_ENUM_ATTR(stablehloCustomCallApiVersionAttrGet, mlir::stablehlo::CustomCallApiVersion, value) 
HS_ENUM_STABLEHLO_CUSTOMECALLAPIVERSIONATTR(API_VERSION_STATUS_RETURNING_UNIFIED)
HS_ENUM_STABLEHLO_CUSTOMECALLAPIVERSIONATTR(API_VERSION_STATUS_RETURNING)
HS_ENUM_STABLEHLO_CUSTOMECALLAPIVERSIONATTR(API_VERSION_UNSPECIFIED)
HS_ENUM_STABLEHLO_CUSTOMECALLAPIVERSIONATTR(API_VERSION_ORIGINAL)


#define HS_ENUM_STABLEHLO_COMPARISIONDIRECTATTR(value) HS_ENUM_ATTR(stablehloComparisonDirectionAttrGet, mlir::stablehlo::ComparisonDirection, value) 
HS_ENUM_STABLEHLO_COMPARISIONDIRECTATTR(EQ)
HS_ENUM_STABLEHLO_COMPARISIONDIRECTATTR(LE)
HS_ENUM_STABLEHLO_COMPARISIONDIRECTATTR(LT)
HS_ENUM_STABLEHLO_COMPARISIONDIRECTATTR(GE)
HS_ENUM_STABLEHLO_COMPARISIONDIRECTATTR(GT)
HS_ENUM_STABLEHLO_COMPARISIONDIRECTATTR(NE)


#define HS_ENUM_STABLEHLO_COMPARISIONTYPEATTR(value) HS_ENUM_ATTR(stablehloComparisonTypeAttrGet, mlir::stablehlo::ComparisonType, value) 
HS_ENUM_STABLEHLO_COMPARISIONTYPEATTR(FLOAT)
HS_ENUM_STABLEHLO_COMPARISIONTYPEATTR(TOTALORDER)
HS_ENUM_STABLEHLO_COMPARISIONTYPEATTR(SIGNED)
HS_ENUM_STABLEHLO_COMPARISIONTYPEATTR(UNSIGNED)
HS_ENUM_STABLEHLO_COMPARISIONTYPEATTR(NOTYPE)

#define HS_ENUM_STABLEHLO_FFTTYPEATTR(value) HS_ENUM_ATTR(stablehloFftTypeAttrGet, mlir::stablehlo::FftType, value)
HS_ENUM_STABLEHLO_FFTTYPEATTR(IRFFT)
HS_ENUM_STABLEHLO_FFTTYPEATTR(FFT)
HS_ENUM_STABLEHLO_FFTTYPEATTR(IFFT)
HS_ENUM_STABLEHLO_FFTTYPEATTR(RFFT)


#define HS_ENUM_STABLEHLO_RNGALGORITHMATTR(value) HS_ENUM_ATTR(stablehloRngAlgorithmAttrGet, mlir::stablehlo::RngAlgorithm, value)
HS_ENUM_STABLEHLO_RNGALGORITHMATTR(THREE_FRY)
HS_ENUM_STABLEHLO_RNGALGORITHMATTR(DEFAULT)
HS_ENUM_STABLEHLO_RNGALGORITHMATTR(PHILOX)

#define HS_ENUM_STABLEHLO_RNGDISTRIBUTIONATTR(value) HS_ENUM_ATTR(stablehloRngDistributionAttrGet, mlir::stablehlo::RngDistribution, value)
HS_ENUM_STABLEHLO_RNGDISTRIBUTIONATTR(NORMAL)
HS_ENUM_STABLEHLO_RNGDISTRIBUTIONATTR(UNIFORM)


#define HS_ENUM_STABLEHLO_TRANSPOSEATTR(value) HS_ENUM_ATTR(stablehloTransposeAttrGet, mlir::stablehlo::Transpose, value)
HS_ENUM_STABLEHLO_TRANSPOSEATTR(NO_TRANSPOSE)
HS_ENUM_STABLEHLO_TRANSPOSEATTR(TRANSPOSE)
HS_ENUM_STABLEHLO_TRANSPOSEATTR(ADJOINT)
HS_ENUM_STABLEHLO_TRANSPOSEATTR(TRANSPOSE_INVALID)

