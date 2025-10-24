#!/bin/sh -l
#
# Resources estimate:
#   C96  :  ~5 minutes per year (  4 tasks), monthly restarts
#   C192 :  ~7 minutes per year ( 16 tasks), monthly restarts
#   C384 :  ~8 minutes per year ( 64 tasks), monthly restarts
#   C768 :  ~? minutes per year ( 64 tasks), monthly restarts
#   C768 :  ~? minutes per year (128 tasks), monthly restarts
#   C1152: ~30 minutes per year (128 tasks), monthly restarts
#
#SBATCH --ntasks=4
#
#SBATCH -q batch
#
#SBATCH --time=0:10:00
#
#SBATCH --account=fv3-cpu
#
#SBATCH --job-name=UFS-Land
#
#SBATCH --chdir=.

################################################################
# case resolution: C96.mx100/C192.mx100/C192.mx025/C384.mx025/C768.mx025/C1152.mx025

case_resolution="C96.mx100"

################################################################
# case name: results will go in directory $case_resolution_$case_name

case_name="test"

################################################################
# case type: daily_output/monthly_restart/daily_restart/hourly_restart
#  defines output type and frequency

case_type="monthly_restart"

################################################################
# simulation start: cold start date [yyyy-mm-dd hh:mm:ss]

simulation_start="1980-01-01 00:00:00"
cold_start_date="1979-12-31_23:00:00"    # one timestep before simulation_start

################################################################
# restart date: leave blank if not a restart [yyyy-mm-dd hh:mm:ss]

restart_date=""

################################################################
# simulation end: end date [yyyy-mm-dd hh:mm:ss]

simulation_end="2025-09-01 00:00:00"

################################################################
# input data base directory, where static/init/datm live

input_directory="/scratch4/NCEPDEV/land/data/ufs-land-driver/"

################################################################
# data atmosphere source: ERA5

datm_source="ERA5"

################################################################
# restart regrid: if this is a restart from a regridded restart

restart_regrid=""                          # e.g., C384.mx025-C1152.mx025, leave blank for no regrid restart
restart_regrid_date="2020-01-01_00-00-00" 
restart_regrid_path="../"                  # path to restart_regrid directory, "../" if located with this script

################################################################
# time step

timestep_seconds=3600

################################################################
# noahmp options: 

dynamic_vegetation_option=4
canopy_stomatal_resistance_option=2
soil_wetness_option=1
runoff_option=1
surface_exchange_option=1
supercooled_soilwater_option=1
frozen_soil_adjust_option=1
radiative_transfer_option=3
snow_albedo_option=1
precip_partition_option=1
soil_temp_lower_bdy_option=2
soil_temp_time_scheme_option=3
thermal_roughness_scheme_option=2
surface_evap_resistance_option=4
glacier_option=2
tq_diagnostic_option=2


################################################################
################################################################
# shouldn't need to change below this line
################################################################
################################################################

case_directory="${case_resolution}_${case_name}"
if [ -d $case_directory ]; then 
  echo "case_directory directory $case_directory already exists"
else
  echo "creating directory: "$case_directory
  mkdir -p $case_directory
fi

cp ufs-land.namelist.template $case_directory/ufs-land.namelist
cd $case_directory

if [ $case_resolution = "C96.mx100" ]; then 
  location_end=18320
elif [ $case_resolution = "C192.mx100" ]; then 
  location_end=69465
elif [ $case_resolution = "C192.mx025" ]; then 
  location_end=70664
elif [ $case_resolution = "C384.mx025" ]; then 
  location_end=272699
elif [ $case_resolution = "C768.mx025" ]; then 
  location_end=1067333
elif [ $case_resolution = "C1152.mx025" ]; then 
  location_end=2381853
else
  echo "ERROR: unknown $case_resolution"
  exit 1
fi

if [ $case_type = "monthly_restart" ]; then 
  restart_frequency_s=-2
  output_frequency_s=0
elif [ $case_type = "daily_restart" ]; then 
  restart_frequency_s=-1
  output_frequency_s=0
elif [ $case_type = "hourly_restart" ]; then 
  restart_frequency_s=3600
  output_frequency_s=0
elif [ $case_type = "daily_output" ]; then 
  restart_frequency_s=0
  output_frequency_s=3600
else
  echo "ERROR: unknown $case_type"
  exit 2
fi

if [[ $restart_date = "" ]]; then 
  restart_simulation=".false."
else
  restart_simulation=".true."
fi
echo "$restart_simulation"

################################################################
# start namelist generation

static_file="${input_directory}vector_inputs/${case_resolution}/ufs-land_${case_resolution}_hr3_static_fields.nc"
init_file="${input_directory}cold_start/${case_resolution}/ERA5-${case_resolution}_hr3_cold_start_$cold_start_date.nc"
forcing_dir="${input_directory}datm/ERA5/${case_resolution}"
forcing_filename_preamble="ERA5-${case_resolution}_hr3_datm_"

sed -i "s,<static_file>,${static_file},g" ufs-land.namelist
sed -i "s,<init_file>,${init_file},g" ufs-land.namelist
sed -i "s,<forcing_dir>,${forcing_dir},g" ufs-land.namelist
sed -i "s/<output_frequency_s>/$output_frequency_s/g" ufs-land.namelist
sed -i "s/<restart_frequency_s>/$restart_frequency_s/g" ufs-land.namelist
sed -i "s/<restart_simulation>/$restart_simulation/g" ufs-land.namelist
sed -i "s/<restart_date>/$restart_date/g" ufs-land.namelist
sed -i "s/<timestep_seconds>/$timestep_seconds/g" ufs-land.namelist
sed -i "s/<simulation_start>/$simulation_start/g" ufs-land.namelist
sed -i "s/<simulation_end>/$simulation_end/g" ufs-land.namelist
sed -i "s/<location_end>/$location_end/g" ufs-land.namelist
sed -i "s/<dynamic_vegetation_option>/$dynamic_vegetation_option/g" ufs-land.namelist
sed -i "s/<canopy_stomatal_resistance_option>/$canopy_stomatal_resistance_option/g" ufs-land.namelist
sed -i "s/<soil_wetness_option>/$soil_wetness_option/g" ufs-land.namelist
sed -i "s/<runoff_option>/$runoff_option/g" ufs-land.namelist
sed -i "s/<surface_exchange_option>/$surface_exchange_option/g" ufs-land.namelist
sed -i "s/<supercooled_soilwater_option>/$supercooled_soilwater_option/g" ufs-land.namelist
sed -i "s/<frozen_soil_adjust_option>/$frozen_soil_adjust_option/g" ufs-land.namelist
sed -i "s/<radiative_transfer_option>/$radiative_transfer_option/g" ufs-land.namelist
sed -i "s/<snow_albedo_option>/$snow_albedo_option/g" ufs-land.namelist
sed -i "s/<precip_partition_option>/$precip_partition_option/g" ufs-land.namelist
sed -i "s/<soil_temp_lower_bdy_option>/$soil_temp_lower_bdy_option/g" ufs-land.namelist
sed -i "s/<soil_temp_time_scheme_option>/$soil_temp_time_scheme_option/g" ufs-land.namelist
sed -i "s/<thermal_roughness_scheme_option>/$thermal_roughness_scheme_option/g" ufs-land.namelist
sed -i "s/<surface_evap_resistance_option>/$surface_evap_resistance_option/g" ufs-land.namelist
sed -i "s/<glacier_option>/$glacier_option/g" ufs-land.namelist
sed -i "s/<tq_diagnostic_option>/$tq_diagnostic_option/g" ufs-land.namelist
sed -i "s/<forcing_filename_preamble>/$forcing_filename_preamble/g" ufs-land.namelist

if [ $restart_regrid != "" ]; then
  ln -sf ${restart_regrid_path}${restart_regrid}/ufs_land_restart.${restart_regrid}_hr3.${restart_regrid_date}.nc ufs_land_restart.${restart_regrid_date}.nc
fi

cp ../../../../ccpp-physics/physics/SFC_Models/Land/Noahmp/noahmptable.tbl .

land_exe="ufsLand.exe.$case_name"

cp ../../../ufsLand.exe $land_exe

echo "starting $land_exe"

module purge
module use /contrib/spack-stack/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/Core
module load stack-oneapi/2024.2.1 
module load stack-intel-oneapi-mpi/2021.13 
module load netcdf-fortran/4.6.1 
module list

time srun -n $SLURM_NTASKS $land_exe
