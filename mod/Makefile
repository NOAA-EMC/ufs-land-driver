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
	lsm_noah.o \
	date_def.o \
	module_sf_noahmplsm.o \
	module_sf_noahmp_glacier.o \
	noahmp_tables.o \
	noahmpdrv.o
	
all:	$(OBJS)

machine.o: $(PHYSDIR)/hooks/machine.F
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/hooks/machine.F

physcons.o: $(PHYSDIR)/hooks/physcons.F90
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/hooks/physcons.F90

funcphys.o: $(PHYSDIR)/tools/funcphys.f90
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/tools/funcphys.f90

surface_perturbation.o: $(PHYSDIR)/SFC_Models/Land/Noah/surface_perturbation.F90
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/SFC_Models/Land/Noah/surface_perturbation.F90

namelist_soilveg.o: $(PHYSDIR)/SFC_Models/Land/Noah/namelist_soilveg.f
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/SFC_Models/Land/Noah/namelist_soilveg.f

set_soilveg.o: $(PHYSDIR)/SFC_Models/Land/Noah/set_soilveg.f
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/SFC_Models/Land/Noah/set_soilveg.f

sflx.o: $(PHYSDIR)/SFC_Models/Land/Noah/sflx.f
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/SFC_Models/Land/Noah/sflx.f

lsm_noah.o: $(PHYSDIR)/SFC_Models/Land/Noah/lsm_noah.f
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/SFC_Models/Land/Noah/lsm_noah.f

date_def.o: $(PHYSDIR)/SFC_Layer/UFS/date_def.f
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/SFC_Layer/UFS/date_def.f

module_sf_noahmp_glacier.o: $(PHYSDIR)/SFC_Models/Land/Noahmp/module_sf_noahmp_glacier.F90
	$(COMPILERF90) -cpp -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/SFC_Models/Land/Noahmp/module_sf_noahmp_glacier.F90

module_sf_noahmplsm.o: $(PHYSDIR)/SFC_Models/Land/Noahmp/module_sf_noahmplsm.F90
	$(COMPILERF90) -cpp -c $(F90FLAGS) -ffree-line-length-none $(FREESOURCE) $(PHYSDIR)/SFC_Models/Land/Noahmp/module_sf_noahmplsm.F90

noahmp_tables.o: $(PHYSDIR)/SFC_Models/Land/Noahmp/noahmp_tables.f90
	$(COMPILERF90) -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/SFC_Models/Land/Noahmp/noahmp_tables.f90

lnd_iau_mod.o: $(PHYSDIR)/SFC_Models/Land/Noahmp/lnd_iau_mod.F90
	$(COMPILERF90) -cpp -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/SFC_Models/Land/Noahmp/lnd_iau_mod.F90

noahmpdrv.o: $(PHYSDIR)/SFC_Models/Land/Noahmp/noahmpdrv.F90
	$(COMPILERF90) -cpp -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/SFC_Models/Land/Noahmp/noahmpdrv.F90

sfc_diff.o: $(PHYSDIR)/SFC_Layer/UFS/sfc_diff.f
	$(COMPILERF90) -cpp -c $(F90FLAGS) $(FREESOURCE) $(PHYSDIR)/SFC_Layer/UFS/sfc_diff.f

clean:
	rm -f *.o *.mod

#
# Dependencies:
#
physcon.o:		        machine.o
funcphys.o:		        machine.o physcons.o
surface_perturbation.o:         machine.o
lnd_iau_mod.o:                  machine.o
set_soilveg.o:		        namelist_soilveg.o
sflx.o: 		        machine.o physcons.o namelist_soilveg.o
lsm_noah.o:		        machine.o physcons.o funcphys.o surface_perturbation.o sflx.o 
sfc_diff.o:		        machine.o
module_sf_noahmp_glacier.o:	module_sf_noahmplsm.o
noahmpdrv.o:	                set_soilveg.o machine.o date_def.o funcphys.o \
                                module_sf_noahmplsm.o lnd_iau_mod.o \
                                module_sf_noahmp_glacier.o noahmp_tables.o

