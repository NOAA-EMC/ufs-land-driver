module ufsLandNoahMPRestartModule

  implicit none
  save
  private

  type, public :: noahmp_restart_type

    character*256    :: filename

  contains

    procedure, public  :: WriteRestartNoahMP
    procedure, public  :: ReadRestartNoahMP

end type noahmp_restart_type
     
contains   

  subroutine WriteRestartNoahMP(this, namelist, noahmp, now_time)
  
  use netcdf
  use time_utilities
  use error_handling, only : handle_err
  use NamelistRead
  use ufsLandNoahMPType

  class(noahmp_restart_type)   :: this  
  type(namelist_type)  :: namelist
  type(noahmp_type)    :: noahmp
  double precision     :: now_time
  character*19     :: nowdate    ! current date
  character*19     :: reference_date = "1970-01-01 00:00:00"
  integer          :: yyyy,mm,dd,hh,nn,ss
  integer :: ncid, dimid, varid, status
  integer :: dim_id_time, dim_id_loc, dim_id_soil, dim_id_snow, dim_id_snso, dim_id_date
  integer :: outsub
  
  outsub = namelist%begsub - namelist%begloc + 1
  
  call date_from_since(reference_date, now_time, nowdate)
  read(nowdate( 1: 4),'(i4.4)') yyyy
  read(nowdate( 6: 7),'(i2.2)') mm
  read(nowdate( 9:10),'(i2.2)') dd
  read(nowdate(12:13),'(i2.2)') hh
  read(nowdate(15:16),'(i2.2)') nn
  read(nowdate(18:19),'(i2.2)') ss

  write(this%filename,'(a17,i4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a3)') &
    "ufs_land_restart.", yyyy, "-", mm, "-", dd, "_", hh, "-", nn, "-", ss, ".nc"

  this%filename = trim(namelist%restart_dir)//"/"//trim(this%filename)

  write(*,*) "Creating: "//trim(this%filename)

  status = nf90_create(this%filename, NF90_CLOBBER, ncid)
    if (status /= nf90_noerr) call handle_err(status)

! Define dimensions in the file.

  status = nf90_def_dim(ncid, "location"    , noahmp%static%vector_length   , dim_id_loc)
    if (status /= nf90_noerr) call handle_err(status)
  status = nf90_def_dim(ncid, "soil_levels" , noahmp%static%soil_levels     , dim_id_soil)
    if (status /= nf90_noerr) call handle_err(status)
  status = nf90_def_dim(ncid, "snow_levels" , 3                             , dim_id_snow)
    if (status /= nf90_noerr) call handle_err(status)
  status = nf90_def_dim(ncid, "snso_levels" , noahmp%static%soil_levels + 3 , dim_id_snso)
    if (status /= nf90_noerr) call handle_err(status)
  status = nf90_def_dim(ncid, "time"        , NF90_UNLIMITED                , dim_id_time)
    if (status /= nf90_noerr) call handle_err(status)
  
! Define variables in the file.

  status = nf90_def_var(ncid, "time", NF90_DOUBLE, dim_id_time, varid)
    status = nf90_put_att(ncid, varid, "long_name", "time")
    status = nf90_put_att(ncid, varid, "units", "seconds since "//reference_date)

  status = nf90_def_var(ncid, "timestep", NF90_DOUBLE, (/dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "time step")
    status = nf90_put_att(ncid, varid, "units", "seconds")

  status = nf90_def_var(ncid, "vegetation_fraction", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "vegetation fraction")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "emissivity_total", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface emissivity")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "albedo_direct_vis", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface albedo - direct visible")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "albedo_direct_nir", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface albedo - direct NIR")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "albedo_diffuse_vis", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface albedo - diffuse visible")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "albedo_diffuse_nir", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface albedo - diffuse NIR")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "temperature_soil_bot", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "deep soil temperature")
    status = nf90_put_att(ncid, varid, "units", "K")

  status = nf90_def_var(ncid, "cm_noahmp", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface exchange coeff for momentum")
    status = nf90_put_att(ncid, varid, "units", "m/s")

  status = nf90_def_var(ncid, "ch_noahmp", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface exchange coeff heat & moisture")
    status = nf90_put_att(ncid, varid, "units", "m/s")

  status = nf90_def_var(ncid, "forcing_height", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "height of forcing")
    status = nf90_put_att(ncid, varid, "units", "m")

  status = nf90_def_var(ncid, "max_vegetation_frac", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "maximum fractional coverage of vegetation")
    status = nf90_put_att(ncid, varid, "units", "fraction")

  status = nf90_def_var(ncid, "albedo_total", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "grid composity albedo")
    status = nf90_put_att(ncid, varid, "units", "fraction")

  status = nf90_def_var(ncid, "snow_water_equiv", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "snow water equivalent")
    status = nf90_put_att(ncid, varid, "units", "mm")

  status = nf90_def_var(ncid, "snow_depth", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "snow depth")
    status = nf90_put_att(ncid, varid, "units", "m")

  status = nf90_def_var(ncid, "temperature_radiative", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface radiative temperature")
    status = nf90_put_att(ncid, varid, "units", "K")

  status = nf90_def_var(ncid, "soil_moisture_vol", NF90_DOUBLE, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "volumetric moisture content in soil level")
    status = nf90_put_att(ncid, varid, "units", "m3/m3")

  status = nf90_def_var(ncid, "temperature_soil", NF90_DOUBLE, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "temperature in soil level")
    status = nf90_put_att(ncid, varid, "units", "K")

  status = nf90_def_var(ncid, "soil_liquid_vol", NF90_DOUBLE, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "volumetric liquid content in soil level")
    status = nf90_put_att(ncid, varid, "units", "m3/m3")

  status = nf90_def_var(ncid, "canopy_water", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "canopy moisture content")
    status = nf90_put_att(ncid, varid, "units", "m")

  status = nf90_def_var(ncid, "transpiration_heat", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "plant transpiration")
    status = nf90_put_att(ncid, varid, "units", "W/m2")

  status = nf90_def_var(ncid, "friction_velocity", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "friction velocity")
    status = nf90_put_att(ncid, varid, "units", "m/s")

  status = nf90_def_var(ncid, "z0_total", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface roughness")
    status = nf90_put_att(ncid, varid, "units", "m")

  status = nf90_def_var(ncid, "snow_cover_fraction", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "snow cover fraction")
    status = nf90_put_att(ncid, varid, "units", "fraction")

  status = nf90_def_var(ncid, "spec_humidity_surface", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "diagnostic specific humidity at sfc")
    status = nf90_put_att(ncid, varid, "units", "kg/kg")

  status = nf90_def_var(ncid, "ground_heat_total", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "soil heat flux")
    status = nf90_put_att(ncid, varid, "units", "W/m2")

  status = nf90_def_var(ncid, "runoff_baseflow", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "drainage runoff")
    status = nf90_put_att(ncid, varid, "units", "mm/s")

  status = nf90_def_var(ncid, "latent_heat_total", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "latent heat flux")
    status = nf90_put_att(ncid, varid, "units", "W/m2")

  status = nf90_def_var(ncid, "sensible_heat_flux", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "sensible heat flux")
    status = nf90_put_att(ncid, varid, "units", "W/m2")

  status = nf90_def_var(ncid, "evaporation_potential", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "potential evaporation")
    status = nf90_put_att(ncid, varid, "units", "mm/s")

  status = nf90_def_var(ncid, "runoff_surface", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface runoff")
    status = nf90_put_att(ncid, varid, "units", "mm/s")

  status = nf90_def_var(ncid, "latent_heat_ground", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "direct soil latent heat flux")
    status = nf90_put_att(ncid, varid, "units", "W/m2")

  status = nf90_def_var(ncid, "latent_heat_canopy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "canopy water latent heat flux")
    status = nf90_put_att(ncid, varid, "units", "W/m2")

  status = nf90_def_var(ncid, "snow_sublimation", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "sublimation/deposit from snowpack")
    status = nf90_put_att(ncid, varid, "units", "mm/s")

  status = nf90_def_var(ncid, "soil_moisture_total", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "total soil column moisture content")
    status = nf90_put_att(ncid, varid, "units", "mm")

  status = nf90_def_var(ncid, "precip_adv_heat_total", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "precipitation advected heat - total")
    status = nf90_put_att(ncid, varid, "units", "W/m2")

  status = nf90_def_var(ncid, "cosine_zenith", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "cosine of zenith angle")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "snow_levels", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "active snow levels")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "temperature_leaf", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "leaf temperature")
    status = nf90_put_att(ncid, varid, "units", "K")

  status = nf90_def_var(ncid, "temperature_ground", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "ground temperature")
    status = nf90_put_att(ncid, varid, "units", "K")

  status = nf90_def_var(ncid, "canopy_ice", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "canopy ice")
    status = nf90_put_att(ncid, varid, "units", "mm")

  status = nf90_def_var(ncid, "canopy_liquid", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "canopy liquid")
    status = nf90_put_att(ncid, varid, "units", "mm")

  status = nf90_def_var(ncid, "vapor_pres_canopy_air", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "temperature_canopy_air", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "cm_noahmp", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "ch_noahmp", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "canopy_wet_fraction", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "fraction of canopy covered by water")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "snow_water_equiv_old", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "snow water equivalent - before integration")
    status = nf90_put_att(ncid, varid, "units", "mm")

  status = nf90_def_var(ncid, "snow_albedo_old", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "snow albedo - before integration")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "snowfall", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "snowfall")
    status = nf90_put_att(ncid, varid, "units", "mm/s")

  status = nf90_def_var(ncid, "lake_water", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "depth_water_table", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "depth to water table")
    status = nf90_put_att(ncid, varid, "units", "m")

  status = nf90_def_var(ncid, "aquifer_water", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "aquifer water content")
    status = nf90_put_att(ncid, varid, "units", "mm")

  status = nf90_def_var(ncid, "saturated_water", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "aquifer + saturated soil water content")
    status = nf90_put_att(ncid, varid, "units", "mm")

  status = nf90_def_var(ncid, "leaf_carbon", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "carbon in leaves")
    status = nf90_put_att(ncid, varid, "units", "g/m2")

  status = nf90_def_var(ncid, "root_carbon", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "carbon in roots")
    status = nf90_put_att(ncid, varid, "units", "g/m2")

  status = nf90_def_var(ncid, "stem_carbon", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "carbon in stems")
    status = nf90_put_att(ncid, varid, "units", "g/m2")

  status = nf90_def_var(ncid, "wood_carbon", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "carbon in wood")
    status = nf90_put_att(ncid, varid, "units", "g/m2")

  status = nf90_def_var(ncid, "soil_carbon_stable", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "stable carbon in soil")
    status = nf90_put_att(ncid, varid, "units", "g/m2")

  status = nf90_def_var(ncid, "soil_carbon_fast", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "fast carbon in soil")
    status = nf90_put_att(ncid, varid, "units", "g/m2")

  status = nf90_def_var(ncid, "leaf_area_index", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "leaf area index")
    status = nf90_put_att(ncid, varid, "units", "m2/m2")

  status = nf90_def_var(ncid, "stem_area_index", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "stem area index")
    status = nf90_put_att(ncid, varid, "units", "m2/m2")

  status = nf90_def_var(ncid, "snow_age", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "BATS non-dimensional snow age")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "soil_moisture_wtd", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "soil water content between bottom of the soil and water table")
    status = nf90_put_att(ncid, varid, "units", "m3/m3")

  status = nf90_def_var(ncid, "deep_recharge", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "deep recharge for runoff_option 5")
    status = nf90_put_att(ncid, varid, "units", "m")

  status = nf90_def_var(ncid, "recharge", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "recharge for runoff_option 5")
    status = nf90_put_att(ncid, varid, "units", "m")

  status = nf90_def_var(ncid, "temperature_2m", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "grid diagnostic temperature at 2 meters")
    status = nf90_put_att(ncid, varid, "units", "K")

  status = nf90_def_var(ncid, "spec_humidity_2m", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "grid diagnostic specific humidity at 2 meters")
    status = nf90_put_att(ncid, varid, "units", "kg/kg")

  status = nf90_def_var(ncid, "eq_soil_water_vol", NF90_DOUBLE, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "equilibrium soil water content")
    status = nf90_put_att(ncid, varid, "units", "m3/m3")

  status = nf90_def_var(ncid, "temperature_snow", NF90_DOUBLE, (/dim_id_loc,dim_id_snow,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "snow level temperature")
    status = nf90_put_att(ncid, varid, "units", "K")

  status = nf90_def_var(ncid, "interface_depth", NF90_DOUBLE, (/dim_id_loc,dim_id_snso,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "layer-bottom depth from snow surface")
    status = nf90_put_att(ncid, varid, "units", "m")

  status = nf90_def_var(ncid, "snow_level_ice", NF90_DOUBLE, (/dim_id_loc,dim_id_snow,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "ice content of snow levels")
    status = nf90_put_att(ncid, varid, "units", "mm")

  status = nf90_def_var(ncid, "snow_level_liquid", NF90_DOUBLE, (/dim_id_loc,dim_id_snow,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "liquid content of snow levels")
    status = nf90_put_att(ncid, varid, "units", "mm")

  status = nf90_enddef(ncid)

! Start writing restart file
  
  status = nf90_inq_varid(ncid, "time", varid)
  status = nf90_put_var(ncid, varid , now_time             )
  
  status = nf90_inq_varid(ncid, "timestep", varid)
  status = nf90_put_var(ncid, varid , noahmp%static%timestep   )

  status = nf90_inq_varid(ncid, "vegetation_fraction", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%vegetation_fraction     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "emissivity_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%emissivity_total        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_direct_vis", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%albedo_direct(:,1)      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_direct_nir", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%albedo_direct(:,2)      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_diffuse_vis", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%albedo_diffuse(:,1)     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_diffuse_nir", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%albedo_diffuse(:,2)     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_soil_bot", varid)
  status = nf90_put_var(ncid, varid , noahmp%static%temperature_soil_bot  , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "cm_noahmp", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%cm_noahmp              , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "ch_noahmp", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%ch_noahmp              , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "forcing_height", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%forcing_height         , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "max_vegetation_frac", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%max_vegetation_frac    , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%albedo_total            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_water_equiv", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%snow_water_equiv       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_depth", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%snow_depth             , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_radiative", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%temperature_radiative  , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_moisture_vol", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%soil_moisture_vol        , &
      start = (/outsub                     ,                         1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels, 1/))

  status = nf90_inq_varid(ncid, "temperature_soil", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%temperature_soil         , &
      start = (/outsub                     ,                         1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels, 1/))

  status = nf90_inq_varid(ncid, "soil_liquid_vol", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%soil_liquid_vol          , &
      start = (/outsub                     ,                         1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels, 1/))

  status = nf90_inq_varid(ncid, "canopy_water", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%canopy_water            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "transpiration_heat", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%transpiration_heat      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "friction_velocity", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%friction_velocity      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "z0_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%z0_total                , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_cover_fraction", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%snow_cover_fraction     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "spec_humidity_surface", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%spec_humidity_surface   , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "ground_heat_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%ground_heat_total       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "runoff_baseflow", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%runoff_baseflow         , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "latent_heat_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%latent_heat_total       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "sensible_heat_flux", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%sensible_heat_total     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "evaporation_potential", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%evaporation_potential   , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "runoff_surface", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%runoff_surface          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "latent_heat_ground", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%latent_heat_ground      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "latent_heat_canopy", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%latent_heat_canopy      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_sublimation", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%snow_sublimation        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_moisture_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%soil_moisture_total     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "precip_adv_heat_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%precip_adv_heat_total   , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "cosine_zenith", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%cosine_zenith          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_levels", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%snow_levels            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_leaf", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%temperature_leaf       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_ground", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%temperature_ground     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "canopy_ice", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%canopy_ice             , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "canopy_liquid", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%canopy_liquid          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "vapor_pres_canopy_air", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%vapor_pres_canopy_air  , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_canopy_air", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%temperature_canopy_air , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "cm_noahmp", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%cm_noahmp              , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "ch_noahmp", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%ch_noahmp              , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "canopy_wet_fraction", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%canopy_wet_fraction     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_water_equiv_old", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%snow_water_equiv_old   , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_albedo_old", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%snow_albedo_old         , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snowfall", varid)
  status = nf90_put_var(ncid, varid , noahmp%forcing%snowfall             , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "lake_water", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%lake_water             , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "depth_water_table", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%depth_water_table       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "aquifer_water", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%aquifer_water          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "saturated_water", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%saturated_water        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "leaf_carbon", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%leaf_carbon            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "root_carbon", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%root_carbon            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "stem_carbon", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%stem_carbon            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "wood_carbon", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%wood_carbon            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_carbon_stable", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%soil_carbon_stable     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_carbon_fast", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%soil_carbon_fast       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "leaf_area_index", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%leaf_area_index        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "stem_area_index", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%stem_area_index        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_age", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%snow_age               , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_moisture_wtd", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%soil_moisture_wtd      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "deep_recharge", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%deep_recharge           , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "recharge", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%recharge                , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_2m", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%temperature_2m          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "spec_humidity_2m", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%spec_humidity_2m        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "eq_soil_water_vol", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%eq_soil_water_vol          , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels  , 1/))

  status = nf90_inq_varid(ncid, "interface_depth", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%interface_depth            , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels+3, 1/))

  status = nf90_inq_varid(ncid, "temperature_snow", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%temperature_snow           , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length,                           3, 1/))

  status = nf90_inq_varid(ncid, "snow_level_ice", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%snow_level_ice             , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length,                           3, 1/))

  status = nf90_inq_varid(ncid, "snow_level_liquid", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%snow_level_liquid          , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length,                           3, 1/))

  status = nf90_close(ncid)

  end subroutine WriteRestartNoahMP
  
  subroutine ReadRestartNoahMP(this, namelist, noahmp)
  
  use netcdf
  use error_handling, only : handle_err
  use time_utilities
  use NamelistRead
  use ufsLandNoahMPType
  
  class(noahmp_restart_type)  :: this
  type(namelist_type)  :: namelist
  type (noahmp_type)   :: noahmp
  double precision     :: now_time
  
  character*19     :: reference_date = "1970-01-01 00:00:00"
  integer          :: yyyy,mm,dd,hh,nn,ss
  integer :: ncid, dimid, varid, status
  integer :: dim_id_time, dim_id_loc, dim_id_soil, dim_id_snow, dim_id_snso, dim_id_date
  integer :: outsub
  
  call calc_sec_since(reference_date,namelist%restart_date,0,now_time)

  namelist%initial_time = now_time

  read(namelist%restart_date( 1: 4),'(i4.4)') yyyy
  read(namelist%restart_date( 6: 7),'(i2.2)') mm
  read(namelist%restart_date( 9:10),'(i2.2)') dd
  read(namelist%restart_date(12:13),'(i2.2)') hh
  read(namelist%restart_date(15:16),'(i2.2)') nn
  read(namelist%restart_date(18:19),'(i2.2)') ss

  write(this%filename,'(a17,i4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a3)') &
    "ufs_land_restart.", yyyy, "-", mm, "-", dd, "_", hh, "-", nn, "-", ss, ".nc"

  this%filename = trim(namelist%restart_dir)//"/"//trim(this%filename)

  write(*,*) "Reading: "//trim(this%filename)
    
  outsub = namelist%begsub - namelist%begloc + 1
  
  status = nf90_open(this%filename, NF90_NOWRITE, ncid)
   if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "time", varid)
  status = nf90_get_var(ncid, varid , now_time)
  
  status = nf90_inq_varid(ncid, "vegetation_fraction", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%vegetation_fraction     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "emissivity_total", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%emissivity_total        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_direct_vis", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%albedo_direct(:,1)      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_direct_nir", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%albedo_direct(:,2)      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_diffuse_vis", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%albedo_diffuse(:,1)     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_diffuse_nir", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%albedo_diffuse(:,2)     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_soil_bot", varid)
  status = nf90_get_var(ncid, varid , noahmp%static%temperature_soil_bot  , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "cm_noahmp", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%cm_noahmp              , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "ch_noahmp", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%ch_noahmp              , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "forcing_height", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%forcing_height         , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "max_vegetation_frac", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%max_vegetation_frac    , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_total", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%albedo_total            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_water_equiv", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%snow_water_equiv       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_depth", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%snow_depth             , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_radiative", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%temperature_radiative  , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_moisture_vol", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%soil_moisture_vol        , &
      start = (/outsub                     ,                         1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels, 1/))

  status = nf90_inq_varid(ncid, "temperature_soil", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%temperature_soil         , &
      start = (/outsub                     ,                         1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels, 1/))

  status = nf90_inq_varid(ncid, "soil_liquid_vol", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%soil_liquid_vol          , &
      start = (/outsub                     ,                         1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels, 1/))

  status = nf90_inq_varid(ncid, "canopy_water", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%canopy_water            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "transpiration_heat", varid)
  status = nf90_get_var(ncid, varid , noahmp%flux%transpiration_heat      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "friction_velocity", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%friction_velocity      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "z0_total", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%z0_total                , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_cover_fraction", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%snow_cover_fraction     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "spec_humidity_surface", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%spec_humidity_surface   , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "ground_heat_total", varid)
  status = nf90_get_var(ncid, varid , noahmp%flux%ground_heat_total       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "runoff_baseflow", varid)
  status = nf90_get_var(ncid, varid , noahmp%flux%runoff_baseflow         , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "latent_heat_total", varid)
  status = nf90_get_var(ncid, varid , noahmp%flux%latent_heat_total       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "sensible_heat_flux", varid)
  status = nf90_get_var(ncid, varid , noahmp%flux%sensible_heat_total     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "evaporation_potential", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%evaporation_potential   , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "runoff_surface", varid)
  status = nf90_get_var(ncid, varid , noahmp%flux%runoff_surface          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "latent_heat_ground", varid)
  status = nf90_get_var(ncid, varid , noahmp%flux%latent_heat_ground      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "latent_heat_canopy", varid)
  status = nf90_get_var(ncid, varid , noahmp%flux%latent_heat_canopy      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_sublimation", varid)
  status = nf90_get_var(ncid, varid , noahmp%flux%snow_sublimation        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_moisture_total", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%soil_moisture_total     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "precip_adv_heat_total", varid)
  status = nf90_get_var(ncid, varid , noahmp%flux%precip_adv_heat_total   , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "cosine_zenith", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%cosine_zenith          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_levels", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%snow_levels            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_leaf", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%temperature_leaf       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_ground", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%temperature_ground     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "canopy_ice", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%canopy_ice             , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "canopy_liquid", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%canopy_liquid          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "vapor_pres_canopy_air", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%vapor_pres_canopy_air  , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_canopy_air", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%temperature_canopy_air , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "cm_noahmp", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%cm_noahmp              , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "ch_noahmp", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%ch_noahmp              , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "canopy_wet_fraction", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%canopy_wet_fraction     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_water_equiv_old", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%snow_water_equiv_old   , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_albedo_old", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%snow_albedo_old         , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snowfall", varid)
  status = nf90_get_var(ncid, varid , noahmp%forcing%snowfall             , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "lake_water", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%lake_water             , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "depth_water_table", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%depth_water_table       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "aquifer_water", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%aquifer_water          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "saturated_water", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%saturated_water        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "leaf_carbon", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%leaf_carbon            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "root_carbon", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%root_carbon            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "stem_carbon", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%stem_carbon            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "wood_carbon", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%wood_carbon            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_carbon_stable", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%soil_carbon_stable     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_carbon_fast", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%soil_carbon_fast       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "leaf_area_index", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%leaf_area_index        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "stem_area_index", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%stem_area_index        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_age", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%snow_age               , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_moisture_wtd", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%soil_moisture_wtd      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "deep_recharge", varid)
  status = nf90_get_var(ncid, varid , noahmp%flux%deep_recharge           , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "recharge", varid)
  status = nf90_get_var(ncid, varid , noahmp%flux%recharge                , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_2m", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%temperature_2m          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "spec_humidity_2m", varid)
  status = nf90_get_var(ncid, varid , noahmp%diag%spec_humidity_2m        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "eq_soil_water_vol", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%eq_soil_water_vol          , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels  , 1/))

  status = nf90_inq_varid(ncid, "temperature_snow", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%temperature_snow           , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length,                           3, 1/))

  status = nf90_inq_varid(ncid, "interface_depth", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%interface_depth            , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels+3, 1/))

  status = nf90_inq_varid(ncid, "snow_level_ice", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%snow_level_ice             , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length,                           3, 1/))

  status = nf90_inq_varid(ncid, "snow_level_liquid", varid)
  status = nf90_get_var(ncid, varid , noahmp%state%snow_level_liquid          , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length,                           3, 1/))
 
  status = nf90_close(ncid)

  end subroutine ReadRestartNoahMP

end module ufsLandNoahMPRestartModule
