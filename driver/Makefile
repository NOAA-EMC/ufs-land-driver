# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90

include ../user_build_config

OBJS =	ufsLandDriver.o \
	ufsLandNoahDriverModule.o \
	ufsLandNoahMPDriverModule.o \
	ufsLandNoahRestartModule.o \
	ufsLandNoahMPRestartModule.o \
	ufsLandIOModule.o \
	ufsLandNamelistRead.o \
	ufsLandNoahType.o \
	ufsLandNoahMPType.o \
	ufsLandInitialModule.o \
	ufsLandForcingModule.o \
	ufsLandStaticModule.o
	
all:	$(OBJS)

.f90.o:
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) -I../mod -I../util $(NETCDFMOD) $(*).f90

clean:
	rm -f *.o *.mod *.exe


#
# Dependencies:
#

ufsLandDriver.o:	ufsLandNoahDriverModule.o ufsLandNoahMPDriverModule.o
ufsLandNoahMPDriverModule.o:	ufsLandNamelistRead.o ufsLandStaticModule.o ufsLandNoahMPType.o \
                        ufsLandInitialModule.o ufsLandForcingModule.o ufsLandIOModule.o \
			ufsLandNoahMPRestartModule.o \
			../util/module_interpolation_utilities.o ../util/module_cosine_zenith.o \
			../util/module_time_utilities.o \
                        ../mod/machine.o  ../mod/set_soilveg.o \
                        ../mod/physcons.o ../mod/noahmp_sfc_drv.o ../mod/funcphys.o
ufsLandNoahDriverModule.o:	ufsLandNamelistRead.o ufsLandStaticModule.o ufsLandNoahType.o \
                        ufsLandInitialModule.o ufsLandForcingModule.o ufsLandIOModule.o \
			ufsLandNoahRestartModule.o \
			../util/module_interpolation_utilities.o \
                        ../mod/machine.o  ../mod/set_soilveg.o \
                        ../mod/physcons.o ../mod/sfc_drv.o ../mod/funcphys.o
ufsLandNoahMPRestartModule.o:	ufsLandNamelistRead.o ufsLandNoahMPType.o
ufsLandNoahRestartModule.o:	ufsLandNamelistRead.o ufsLandNoahType.o
ufsLandIOModule.o:	ufsLandNamelistRead.o ufsLandNoahType.o ufsLandNoahMPType.o \
                        ../util/module_error_handling.o ../util/module_time_utilities.o
ufsLandNoahType.o:	ufsLandNamelistRead.o 
ufsLandNoahMPType.o:	ufsLandNamelistRead.o ../mod/noahmp_tables.o
ufsLandInitialModule.o:	ufsLandNamelistRead.o ufsLandNoahType.o ufsLandNoahMPType.o ../util/module_error_handling.o
ufsLandStaticModule.o:	ufsLandNamelistRead.o ufsLandNoahType.o ufsLandNoahMPType.o ../util/module_error_handling.o
ufsLandForcingModule.o:	ufsLandNamelistRead.o ufsLandNoahType.o ufsLandNoahMPType.o \
                        ../util/module_error_handling.o	../util/module_time_utilities.o \
			ufsLandStaticModule.o
ufsLandNamelistRead.o:	../util/module_time_utilities.o
