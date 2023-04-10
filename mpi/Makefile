# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_config

OBJS =  mpi_land.o 

all:	$(OBJS)

mpi_land.o: mpi_land.f90
	$(COMPILERF90) -cpp $(F90FLAGS) $(FREESOURCE) -c $(*).f90

clean:
	rm -f  *.o *.mod 
