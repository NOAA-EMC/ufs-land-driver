#!/bin/sh -l
#
# -- Request n cores
#SBATCH --ntasks=4
#
# -- Specify queue
#SBATCH -q debug
#
# -- Specify a maximum wallclock
#SBATCH --time=0:30:00
#
# -- Specify under which account a job should run
#SBATCH --account=fv3-cpu
#
# -- Set the name of the job, or Slurm will default to the name of the script
#SBATCH --job-name=UFS-Land
#
# -- Tell the batch system to set the working directory to the current working directory
#SBATCH --chdir=.

module purge
module load stack-intel/2021.5.0  
module load stack-intel-oneapi-mpi/2021.5.1
module load netcdf-hdf5parallel/4.7.4
module load nco/5.0.6
module load ncl/6.6.2_spack

testname="baseline_hr3"
compare_to="baseline_hr3"
table="/scratch2/NCEPDEV/land/data/conus/tables/noahmptable.tbl.baseline"
restart_version="hr3"

mkdir $testname
cd $testname

ln -s -f ../../namelists/ufs-land.namelist.$testname ufs-land.namelist
cp $table ./noahmptable.tbl

time srun -n $SLURM_NTASKS ../../../../ufsLand.exe

ncra ufs_land_monthly_mean.201[3-9]-01.nc ufs_land_monthly_mean.202[0-2]-01.nc  ufs_land_monthly_mean.2013-2022.01.nc
ncra ufs_land_monthly_mean.201[3-9]-02.nc ufs_land_monthly_mean.202[0-2]-02.nc  ufs_land_monthly_mean.2013-2022.02.nc
ncra ufs_land_monthly_mean.201[3-9]-03.nc ufs_land_monthly_mean.202[0-2]-03.nc  ufs_land_monthly_mean.2013-2022.03.nc
ncra ufs_land_monthly_mean.201[3-9]-04.nc ufs_land_monthly_mean.202[0-2]-04.nc  ufs_land_monthly_mean.2013-2022.04.nc
ncra ufs_land_monthly_mean.201[3-9]-05.nc ufs_land_monthly_mean.202[0-2]-05.nc  ufs_land_monthly_mean.2013-2022.05.nc
ncra ufs_land_monthly_mean.201[3-9]-06.nc ufs_land_monthly_mean.202[0-2]-06.nc  ufs_land_monthly_mean.2013-2022.06.nc
ncra ufs_land_monthly_mean.201[3-9]-07.nc ufs_land_monthly_mean.202[0-2]-07.nc  ufs_land_monthly_mean.2013-2022.07.nc
ncra ufs_land_monthly_mean.201[3-9]-08.nc ufs_land_monthly_mean.202[0-2]-08.nc  ufs_land_monthly_mean.2013-2022.08.nc
ncra ufs_land_monthly_mean.201[3-9]-09.nc ufs_land_monthly_mean.202[0-2]-09.nc  ufs_land_monthly_mean.2013-2022.09.nc
ncra ufs_land_monthly_mean.201[3-9]-10.nc ufs_land_monthly_mean.202[0-2]-10.nc  ufs_land_monthly_mean.2013-2022.10.nc
ncra ufs_land_monthly_mean.201[3-9]-11.nc ufs_land_monthly_mean.202[0-2]-11.nc  ufs_land_monthly_mean.2013-2022.11.nc
ncra ufs_land_monthly_mean.201[3-9]-12.nc ufs_land_monthly_mean.202[0-2]-12.nc  ufs_land_monthly_mean.2013-2022.12.nc

ncra ufs_land_diurnal.201[3-9]-01.nc ufs_land_diurnal.202[0-2]-01.nc  ufs_land_diurnal.2013-2022.01.nc
ncra ufs_land_diurnal.201[3-9]-02.nc ufs_land_diurnal.202[0-2]-02.nc  ufs_land_diurnal.2013-2022.02.nc
ncra ufs_land_diurnal.201[3-9]-03.nc ufs_land_diurnal.202[0-2]-03.nc  ufs_land_diurnal.2013-2022.03.nc
ncra ufs_land_diurnal.201[3-9]-04.nc ufs_land_diurnal.202[0-2]-04.nc  ufs_land_diurnal.2013-2022.04.nc
ncra ufs_land_diurnal.201[3-9]-05.nc ufs_land_diurnal.202[0-2]-05.nc  ufs_land_diurnal.2013-2022.05.nc
ncra ufs_land_diurnal.201[3-9]-06.nc ufs_land_diurnal.202[0-2]-06.nc  ufs_land_diurnal.2013-2022.06.nc
ncra ufs_land_diurnal.201[3-9]-07.nc ufs_land_diurnal.202[0-2]-07.nc  ufs_land_diurnal.2013-2022.07.nc
ncra ufs_land_diurnal.201[3-9]-08.nc ufs_land_diurnal.202[0-2]-08.nc  ufs_land_diurnal.2013-2022.08.nc
ncra ufs_land_diurnal.201[3-9]-09.nc ufs_land_diurnal.202[0-2]-09.nc  ufs_land_diurnal.2013-2022.09.nc
ncra ufs_land_diurnal.201[3-9]-10.nc ufs_land_diurnal.202[0-2]-10.nc  ufs_land_diurnal.2013-2022.10.nc
ncra ufs_land_diurnal.201[3-9]-11.nc ufs_land_diurnal.202[0-2]-11.nc  ufs_land_diurnal.2013-2022.11.nc
ncra ufs_land_diurnal.201[3-9]-12.nc ufs_land_diurnal.202[0-2]-12.nc  ufs_land_diurnal.2013-2022.12.nc

mkdir /scratch2/NCEPDEV/land/data/conus/tests/$testname

cp *2013-2022*.nc /scratch2/NCEPDEV/land/data/conus/tests/$testname

cmdparm="'sim1="\"$compare_to"\"' 'sim2="\"$testname"\"' "

eval "ncl ../../plot_scripts/plot_diurnal.ncl $cmdparm"

eval "ncl ../../plot_scripts/plot_monthly_et_vegtype.ncl $cmdparm"

eval "ncl ../../plot_scripts/plot_monthly_et_bukovsky1.ncl $cmdparm"

eval "ncl ../../plot_scripts/plot_monthly_et_bukovsky2.ncl $cmdparm"

eval "ncl ../../plot_scripts/plot_monthly_snow_vegtype.ncl $cmdparm"

eval "ncl ../../plot_scripts/plot_monthly_snow_bukovsky.ncl $cmdparm"

eval "ncl ../../plot_scripts/plot_monthly_sm_bukovsky1.ncl $cmdparm"

eval "ncl ../../plot_scripts/plot_monthly_sm_bukovsky2.ncl $cmdparm"

eval "ncl ../../plot_scripts/plot_monthly_st_bukovsky1.ncl $cmdparm"

eval "ncl ../../plot_scripts/plot_monthly_st_bukovsky2.ncl $cmdparm"

cp *.png /scratch2/NCEPDEV/land/data/conus/tests/$testname

cp ../../namelists/ufs-land.namelist.$testname /scratch2/NCEPDEV/land/data/conus/tests/$testname
