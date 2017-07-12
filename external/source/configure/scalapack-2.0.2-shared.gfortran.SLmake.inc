############################################################################
#
#  Program:         ScaLAPACK
#
#  Module:          SLmake.inc
#
#  Purpose:         Top-level Definitions
#
#  Creation date:   February 15, 2000
#
#  Modified:        October 13, 2011
#
#  Send bug reports, comments or suggestions to scalapack@cs.utk.edu
#
############################################################################
#
#  C preprocessor definitions:  set CDEFS to one of the following:
#
#     -DNoChange (fortran subprogram names are lower case without any suffix)
#     -DUpCase   (fortran subprogram names are upper case without any suffix)
#     -DAdd_     (fortran subprogram names are lower case with "_" appended)

CDEFS         = -DAdd_

#
#  The fortran and C compilers, loaders, and their flags
#

MPI_ROOT      = $(HOME)/program/mpich/mpich2-1.5-gfortran-5.4.0
FC            = $(MPI_ROOT)/bin/mpif90
CC            = $(MPI_ROOT)/bin/mpicc 
NOOPT         = -O0 -fPIC
FCFLAGS       = -O3 -fPIC
CCFLAGS       = -O3 -fPIC
FCLOADER      = $(FC)
CCLOADER      = $(CC)
FCLOADFLAGS   = $(FCFLAGS)
CCLOADFLAGS   = $(CCFLAGS)

#
#  The archiver and the flag(s) to use when building archive (library)
#  Also the ranlib routine.  If your system has no ranlib, set RANLIB = echo
#

ARCH          = ar
ARCHFLAGS     = cr
RANLIB        = ranlib

#
#  The name of the ScaLAPACK library to be created
#

SCALAPACKLIB  = libscalapack.a

#
#  BLAS, LAPACK (and possibly other) libraries needed for linking test programs
#

LAPACK_ROOT   = $(HOME)/program/MOMAP/MOMAP-v1.0/gfortran-debug/external/lapack/lapack-3.7.0-shared
BLASLIB       = -lblas
LAPACKLIB     = -llapack
LIBS          = -L$(LAPACK_ROOT) $(LAPACKLIB) $(BLASLIB)
