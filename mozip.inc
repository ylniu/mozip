#-----------------------------------------------------------------------
PROGRAM        = /d/Programs/MOZIP/mozip
VERSION        = 1.0
MOZIPDIR       = /d/Programs/MOZIP/mozip
MOZIPINC       = mozip.inc
MPROG          = lib transform
MAINSH         = mozip
#-----------------------------------------------------------------------
PARA           = YES
#-----------------------------------------------------------------------
MPI_ROOT       = 
#-----------------------------------------------------------------------
OS             = WIN
DEF            = 
FC             = ifort
FCP            = mpif90
CC             = icc
CCP            = mpicc
OPENMP         = -openmp
OPTS           = -O3
DP8            = -Qautodouble
DP16           = 
RX             = -Qautodouble
FFLAG          = -Qautodouble -debug:full -traceback -check:all
FCFLAG         = -c -Qautodouble -debug:full -traceback -check:all
AR             = xilink -lib
AROUT          = /out:
RM             = rm -rf
LN             = cp
MKLDIR         = D:\programs\IntelSWTools\parallel_studio_xe_2017\compilers_and_libraries_2017\windows\mkl
STATICEXT      = lib
SHAREDEXT      = dll
OBJEXT         = obj
DFLAG          = /ID:\programs\IntelSWTools\parallel_studio_xe_2017\compilers_and_libraries_2017\windows\compiler\include\intel64 -fpp -DIFORT -DWIN
INCPATH        = D:\programs\IntelSWTools\parallel_studio_xe_2017\compilers_and_libraries_2017\windows\compiler\include\intel64
#-----------------------------------------------------------------------
FFTW_ROOT      = /d/Programs/MOZIP/mozip/external/fftw/fftw-3.3.6-pl1-shared
LAPACK_ROOT    = /d/Programs/MOZIP/mozip/external/lapack/lapack-3.7.0-shared
SCALAPACK_ROOT = /d/Programs/MOZIP/mozip/external/scalapack/scalapack-2.0.2-shared
#-----------------------------------------------------------------------
INCDIR         = /d/Programs/MOZIP/mozip/include
LIBDIR         = /d/Programs/MOZIP/mozip/lib
BINDIR         = /d/Programs/MOZIP/mozip/bin
#-----------------------------------------------------------------------
EXAMPLES       = /d/Programs/MOZIP/mozip/examples
NODEFILE       = /d/Programs/MOZIP/mozip/examples/config/nodefile
#-----------------------------------------------------------------------
FFTW           = -I/d/Programs/MOZIP/mozip/external/fftw/fftw-3.3.6-pl1-shared/include -L/d/Programs/MOZIP/mozip/external/fftw/fftw-3.3.6-pl1-shared/lib -L/d/Programs/MOZIP/mozip/external/fftw/fftw-3.3.6-pl1-shared/lib64 -lfftw3
FFTWINSTALL    = /d/Programs/MOZIP/mozip/external/fftw/fftw-3.3.6-pl1-shared
#-----------------------------------------------------------------------
FLIB           = -lflib
FLIBREL        = lib/libflib.a
LAPACK         = mkl_intel_lp64.lib mkl_intel_thread.lib mkl_core.lib
ISMKL          = yes
LAPACKREL      = external/lapack/lapack-3.7.0-shared/liblapack.dll
BLASREL        = external/lapack/lapack-3.7.0-shared/libblas.dll
SCALAPACK      = -L/d/Programs/MOZIP/mozip/external/scalapack/scalapack-2.0.2-shared -lscalapack
#-----------------------------------------------------------------------
GTOOLDIR    = /d/Programs/MOZIP/mozip/grouptools
GTBIN       = /d/Programs/MOZIP/mozip/grouptools/bin
GPROG       = chemshell gaussian ivr molcas molpro public quantum_espresso wien2k turbomole
