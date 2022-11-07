# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_config

OBJS =	../driver/ufsLandDriver.o \
	../driver/ufsLandNoahDriverModule.o \
	../driver/ufsLandNoahMPDriverModule.o \
	../driver/ufsLandNoahRestartModule.o \
	../driver/ufsLandNoahMPRestartModule.o \
	../driver/ufsLandNamelistRead.o \
	../driver/ufsLandNoahType.o \
	../driver/ufsLandNoahMPType.o \
	../driver/ufsLandGenericType.o \
	../driver/ufsLandStaticModule.o \
	../driver/ufsLandInitialModule.o \
	../driver/ufsLandIOModule.o \
	../driver/ufsLandForcingModule.o \
	../driver/ufsLandNetcdf.o \
	../driver/ufsLandGenericIO.o \
	../util/module_error_handling.o \
	../util/module_time_utilities.o \
	../util/module_interpolation_utilities.o \
	../util/module_cosine_zenith.o \
	../mpi/mpi_land.o \
	../mod/machine.o \
	../mod/physcons.o \
	../mod/funcphys.o \
	../mod/surface_perturbation.o \
	../mod/namelist_soilveg.o \
	../mod/set_soilveg.o \
	../mod/sflx.o \
	../mod/lsm_noah.o \
	../mod/date_def.o \
	../mod/module_sf_noahmp_glacier.o \
	../mod/module_sf_noahmplsm.o \
	../mod/noahmp_tables.o \
	../mod/noahmpdrv.o \
	../mod/sfc_diff.o

all:	ufsLand.exe

ufsLand.exe: $(OBJS)
	$(COMPILERF90) -o $(@) $(F90FLAGS) $(FREESOURCE) \
	-I../driver -I../mod -I../util -I../mpi $(NETCDFMOD) $(OBJS) $(NETCDFLIB)

clean:
	rm -f *.exe

