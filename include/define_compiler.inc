#ifndef __COMPILER__
#define  __INTEL_COMP        1
#define  __GFORTRAN_COMP     2
#define  __NVIDIA_COMP       3
#define  __NAG_COMP          4
#define  __LLVM_FLANG_COMP   5
#define  __FLANG_COMP        6
#define  __LFORTRAN_COMP     7
#define  __UNKNOWN_COMP   9999

#define FLOAT128
#undef  HAS_DT

#ifdef __INTEL_COMPILER
#   define __COMPILER__ __INTEL_COMP
#   define HAS_DT
#elif __GFORTRAN__ == 1
#   define __COMPILER__ __GFORTRAN_COMP
#   define HAS_DT
#elif __FLANG
#   undef FLOAT128
#   warning  NOTE: FLOAT128 not supported
#   undef HAS_DT
#   define __COMPILER__ __FLANG_COMP
#elif __flang__
#   undef FLOAT128
#   warning  NOTE: FLOAT128 not supported
#   undef HAS_DT
#   define __COMPILER__ __LLVM_FLANG_COMP
#elif __NVCOMPILER
#   undef HAS_DT
#   undef FLOAT128
#   warning  NOTE: FLOAT128 not supported
#   define __COMPILER__ __NVIDIA_COMP
#elif __NVCOMPILER_LLVM__
#   undef FLOAT128
#   warning  NOTE: FLOAT128 not supported
#   define __COMPILER__ __NVIDIA_COMP
#elif __LFORTRAN
#   define __COMPILER__ __LFORTRAN_COMP
#   undef FLOAT128
#elif __NVCOMPILER
#   undef FLOAT128
#   warning  NOTE: FLOAT128 not supported
#   define __COMPILER__ __NVIDIA_COMP
#else
#   undef FLOAT128
#   warning  NOTE: FLOAT128 not supported
#   define __COMPILER__ __UNKNOWN_COMP
#   warning  NOTE: UNKNOWN COMPILER
#endif
#endif
