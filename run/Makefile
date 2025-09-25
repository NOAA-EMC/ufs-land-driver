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
	../driver/ufsLandOutputMean.o \
	../driver/ufsLandOutputTime.o \
	../utility_code/module_error_handling.o \
	../utility_code/module_time_utilities.o \
	../utility_code/module_interpolation_utilities.o \
	../utility_code/module_cosine_zenith.o \
	../utility_code/module_diagnostics.o \
	../mpi/mpi_land.o \
	../mod/machine.o \
	../mod/physcons.o \
	../mod/funcphys.o \
	../mod/surface_perturbation.o \
	../mod/lnd_iau_mod.o \
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
	-I../driver -I../mod -I../utility_code -I../mpi $(NETCDFMOD) $(OBJS) $(NETCDFLIB)

clean:
	rm -f *.exe

