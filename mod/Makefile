# Makefile 
#
.SUFFIXES:
.SUFFIXES: .o .f90 .F90 .f .F

include ../user_build_config

OBJS =	machine.o \
	physcons.o \
	funcphys.o \
	surface_perturbation.o \
	namelist_soilveg.o \
	set_soilveg.o \
	sflx.o \
	sfc_diff.o \
	sfc_drv.o \
	date_def.o \
	module_sf_noahmp_glacier.o \
	module_sf_noahmplsm.o \
	noahmp_tables.o \
	sfc_noahmp_drv.o
	
all:	$(OBJS)

machine.o: $(PHYSDIR)/machine.F
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/machine.F

physcons.o: $(PHYSDIR)/physcons.F90
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/physcons.F90

funcphys.o: $(PHYSDIR)/funcphys.f90
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/funcphys.f90

surface_perturbation.o: $(PHYSDIR)/surface_perturbation.F90
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/surface_perturbation.F90

namelist_soilveg.o: $(PHYSDIR)/namelist_soilveg.f
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/namelist_soilveg.f

set_soilveg.o: $(PHYSDIR)/set_soilveg.f
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/set_soilveg.f

sflx.o: $(PHYSDIR)/sflx.f
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/sflx.f

sfc_drv.o: $(PHYSDIR)/sfc_drv.f
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/sfc_drv.f

date_def.o: $(PHYSDIR)/date_def.f
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/date_def.f

module_sf_noahmp_glacier.o: $(PHYSDIR)/module_sf_noahmp_glacier.f90
	$(COMPILERF90) -cpp -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/module_sf_noahmp_glacier.f90

module_sf_noahmplsm.o: $(PHYSDIR)/module_sf_noahmplsm.f90
	$(COMPILERF90) -cpp -c $(F90FLAGS) -ffree-line-length-none $(FREESOURCE) $(PHYSDIR)/module_sf_noahmplsm.f90

noahmp_tables.o: $(PHYSDIR)/noahmp_tables.f90
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/noahmp_tables.f90

sfc_noahmp_drv.o: $(PHYSDIR)/sfc_noahmp_drv.F90
	$(COMPILERF90) -cpp -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/sfc_noahmp_drv.F90

sfc_diff.o: $(PHYSDIR)/sfc_diff.f
	$(COMPILERF90) -cpp -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/sfc_diff.f

clean:
	rm -f *.o *.mod

#
# Dependencies:
#
physcon.o:		machine.o
funcphys.o:		machine.o physcons.o
surface_perturbation.o: machine.o
set_soilveg.o:		namelist_soilveg.o
sflx.o: 		machine.o physcons.o namelist_soilveg.o
sfc_drv.o:		machine.o physcons.o funcphys.o surface_perturbation.o sflx.o 
sfc_diff.o:		machine.o
noahmp_sfc_drv.o:	set_soilveg.o machine.o date_def.o funcphys.o module_sf_noahmplsm.o \
                        module_sf_noahmp_glacier.o noahmp_tables.o

