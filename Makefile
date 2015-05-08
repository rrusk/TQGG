# ============================================================================
# Name        : Makefile
# Author      : Raymond Rusk
# Version     :
# Copyright   : Copyright (C) 2012 under GNU General Public License
# Description : Makefile for TQGridGen  (needs a lot of work yet)
# ============================================================================

.PHONY: all clean

# Change these lines if you are using different
# Fortran or C compilers and compiler options.
FC = gfortran
F_FLAGS = -Wall -Wtabs -fdefault-real-8 # -O2 -g
CC = gcc
C_FLAGS = -Wall
LIBS = -lXm -lXt -lX11

TQGG:
	$(FC) $(F_FLAGS) -c \
		src/dataio/MainArrays.f90 \
		src/gridgen/*.f90 \
		src/plotsubs/*.f90 \
		src/PigInterface/*.f90 \
		src/gridedit/*.f src/gridedit/*.f90 \
		src/dataio/*.f src/dataio/*.f90 \
		src/dataio/*.F90 \
		src/XMotif/*.f90
	$(CC) $(C_FLAGS) -c src/XMotif/f90_c.h \
		src/XMotif/XmMain.h src/XMotif/XmMain.c \
		src/XMotif/XmDrawdbl.c src/XMotif/XmDialogs.c
	mkdir -p ./bin
	$(FC) *.o -o bin/TQGG $(LIBS)

TQGGnc:
	$(FC) $(F_FLAGS) -c -DCNCD -I/usr/local/include \
		src/dataio/MainArrays.f90 \
		src/gridgen/*.f90 \
		src/plotsubs/*.f90 \
		src/PigInterface/*.f90 \
		src/gridedit/*.f src/gridedit/*.f90 \
		src/dataio/*.f src/dataio/*.f90 \
		src/dataio/*.f src/dataio/*.F90 \
		src/netcdf/UGrid_io_netCDF.f90 \
		src/netcdf/ReadWriteCDF.f90 \
		src/XMotif/*.f90
	$(CC) $(C_FLAGS) -c src/XMotif/f90_c.h \
		src/XMotif/XmMain.h src/XMotif/XmMain.c \
		src/XMotif/XmDrawdbl.c src/XMotif/XmDialogs.c
	mkdir -p ./bin
	$(FC) *.o -o bin/TQGGnc -lnetcdff -lnetcdf $(LIBS)

clean:
	rm -f bin/TQGGdbl *.o *.mod
