# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_config

OBJS =	module_time_utilities.o \
        module_interpolation_utilities.o \
	module_cosine_zenith.o \
	module_error_handling.o
	
all:	$(OBJS)

.f90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(NETCDFMOD) $(*).f90

clean:
	rm -f *.o *.mod *.exe


#
# Dependencies:
#

module_time_utilities.o:
module_error_handling.o:
module_interpolation_utilities.o: module_time_utilities.o module_cosine_zenith.o
module_cosine_zenith.o: module_time_utilities.o
