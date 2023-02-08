#!/bin/sh -l
#
# -- Request n cores
#SBATCH --ntasks=4
#
# -- Specify queue
#SBATCH -q debug
#
# -- Specify a maximum wallclock
#SBATCH --time=0:05:00
#
# -- Specify under which account a job should run
#SBATCH --account=fv3-cpu
#
# -- Set the name of the job, or Slurm will default to the name of the script
#SBATCH --job-name=UFS-Land
#
# -- Tell the batch system to set the working directory to the current working directory
#SBATCH --chdir=.

nt=$SLURM_NTASKS

module load intel/2022.2.0
module load impi/2022.2.0
module load netcdf-hdf5parallel/4.7.4 

ln -sf ufs-land.namelist.era5.C96 ufs-land.namelist

time srun -n $nt ../../ufsLand.exe
