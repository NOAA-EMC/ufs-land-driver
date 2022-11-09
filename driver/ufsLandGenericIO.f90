module ufsLandGenericIO

  implicit none
  save
  
  integer, parameter, private :: output = 1, restart = 2

contains   

  subroutine DefineNoahMP(io_type, noahmp, ncid, &
                          dim_id_time, dim_id_loc, dim_id_soil, &
                          dim_id_snow, dim_id_snso, dim_id_date, dim_id_rad)
  
  use netcdf, only : NF90_DOUBLE, NF90_FLOAT, NF90_INT
  use ufsLandNoahMPType
  use ufsLandNetcdf
  
  type(noahmp_type)    :: noahmp

  integer :: io_type, realtype
  integer :: ncid
  integer :: dim_id_time, dim_id_loc, dim_id_soil, dim_id_snow, dim_id_snso, dim_id_date, dim_id_rad
  
  io_setup : select case(io_type)
  
    case(output)
    
      realtype = NF90_FLOAT    ! write output as single precision

    case(restart)
    
      realtype = NF90_DOUBLE   ! write restart as double precision

    case default

      stop "io_type out of range in DefineNoahMP"

  end select io_setup

! Begin noahmp%static variables

  if((noahmp%static%vegetation_category%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%vegetation_category%output_flag  .and. io_type == output )) &
    call Define1dInt(noahmp%static%vegetation_category, ncid, NF90_INT, dim_id_loc, dim_id_time)

  if((noahmp%static%soil_category%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%soil_category%output_flag  .and. io_type == output )) &
    call Define1dInt(noahmp%static%soil_category, ncid, NF90_INT, dim_id_loc, dim_id_time)

  if((noahmp%static%slope_category%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%slope_category%output_flag  .and. io_type == output )) &
    call Define1dInt(noahmp%static%slope_category, ncid, NF90_INT, dim_id_loc, dim_id_time)

  if((noahmp%static%soil_interface_depth%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%soil_interface_depth%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%static%soil_interface_depth, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%static%ice_flag%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%ice_flag%output_flag  .and. io_type == output )) &
    call Define1dInt(noahmp%static%ice_flag, ncid, NF90_INT, dim_id_loc, dim_id_time)

  if((noahmp%static%surface_type%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%surface_type%output_flag  .and. io_type == output )) &
    call Define1dInt(noahmp%static%surface_type, ncid, NF90_INT, dim_id_loc, dim_id_time)

  if((noahmp%static%crop_type%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%crop_type%output_flag  .and. io_type == output )) &
    call Define1dInt(noahmp%static%crop_type, ncid, NF90_INT, dim_id_loc, dim_id_time)

  if((noahmp%static%temperature_soil_bot%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%temperature_soil_bot%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%static%temperature_soil_bot, ncid, realtype, dim_id_loc, dim_id_time)

! Begin noahmp%model variables

  if((noahmp%model%latitude%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%latitude%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%latitude, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%cosine_zenith%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%cosine_zenith%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%cosine_zenith, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%forcing_height%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%forcing_height%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%forcing_height, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%vegetation_fraction%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%vegetation_fraction%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%vegetation_fraction, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%max_vegetation_frac%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%max_vegetation_frac%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%max_vegetation_frac, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%snow_levels%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%snow_levels%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%snow_levels, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%interface_depth%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%interface_depth%output_flag  .and. io_type == output )) &
    call Define2dReal(noahmp%model%interface_depth, ncid, realtype, dim_id_loc, dim_id_snso, dim_id_time)

  if((noahmp%model%snow_soil_thickness%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%snow_soil_thickness%output_flag  .and. io_type == output )) &
    call Define2dReal(noahmp%model%snow_soil_thickness, ncid, realtype, dim_id_loc, dim_id_snso, dim_id_time)

  if((noahmp%model%leaf_area_index%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%leaf_area_index%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%leaf_area_index, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%stem_area_index%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%stem_area_index%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%stem_area_index, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%growing_deg_days%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%growing_deg_days%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%growing_deg_days, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%plant_growth_stage%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%plant_growth_stage%output_flag  .and. io_type == output )) &
    call Define1dInt(noahmp%model%plant_growth_stage, ncid, NF90_INT, dim_id_loc, dim_id_time)

  if((noahmp%model%cm_noahmp%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%cm_noahmp%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%cm_noahmp, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%ch_noahmp%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%ch_noahmp%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%ch_noahmp, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%ch_vegetated%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%ch_vegetated%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%ch_vegetated, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%ch_bare_ground%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%ch_bare_ground%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%ch_bare_ground, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%ch_leaf%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%ch_leaf%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%ch_leaf, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%ch_below_canopy%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%ch_below_canopy%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%ch_below_canopy, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%ch_vegetated_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%ch_vegetated_2m%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%ch_vegetated_2m, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%ch_bare_ground_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%ch_bare_ground_2m%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%ch_bare_ground_2m, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%friction_velocity%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%friction_velocity%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%friction_velocity, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%rs_sunlit%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%rs_sunlit%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%rs_sunlit, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%rs_shaded%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%rs_shaded%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%rs_shaded, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%leaf_air_resistance%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%leaf_air_resistance%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%leaf_air_resistance, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%pbl_height%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%pbl_height%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%pbl_height, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%mo_length_inverse%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%mo_length_inverse%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%mo_length_inverse, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%heat_flux_multiplier%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%heat_flux_multiplier%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%heat_flux_multiplier, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%model%moisture_flux_multiplier%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%moisture_flux_multiplier%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%model%moisture_flux_multiplier, ncid, realtype, dim_id_loc, dim_id_time)

! Begin noahmp%forcing variables

  if((noahmp%forcing%temperature_forcing%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%temperature_forcing%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%forcing%temperature_forcing, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%forcing%specific_humidity_forcing%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%specific_humidity_forcing%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%forcing%specific_humidity_forcing, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%forcing%surface_pressure_forcing%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%surface_pressure_forcing%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%forcing%surface_pressure_forcing, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%forcing%wind_speed_forcing%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%wind_speed_forcing%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%forcing%wind_speed_forcing, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%forcing%downward_longwave_forcing%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%downward_longwave_forcing%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%forcing%downward_longwave_forcing, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%forcing%downward_shortwave_forcing%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%downward_shortwave_forcing%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%forcing%downward_shortwave_forcing, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%forcing%precipitation_forcing%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%precipitation_forcing%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%forcing%precipitation_forcing, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%forcing%precip_convective%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%precip_convective%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%forcing%precip_convective, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%forcing%precip_non_convective%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%precip_non_convective%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%forcing%precip_non_convective, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%forcing%precip_snow%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%precip_snow%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%forcing%precip_snow, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%forcing%precip_graupel%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%precip_graupel%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%forcing%precip_graupel, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%forcing%precip_hail%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%precip_hail%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%forcing%precip_hail, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%forcing%snowfall%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%snowfall%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%forcing%snowfall, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%forcing%rainfall%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%rainfall%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%forcing%rainfall, ncid, realtype, dim_id_loc, dim_id_time)

! Begin noahmp%diag variables

  if((noahmp%diag%z0_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%z0_total%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%z0_total, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%z0h_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%z0h_total%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%z0h_total, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%albedo_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%albedo_total%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%albedo_total, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%albedo_direct%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%albedo_direct%output_flag  .and. io_type == output )) &
    call Define2dReal(noahmp%diag%albedo_direct, ncid, realtype, dim_id_loc, dim_id_rad, dim_id_time)

  if((noahmp%diag%albedo_diffuse%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%albedo_diffuse%output_flag  .and. io_type == output )) &
    call Define2dReal(noahmp%diag%albedo_diffuse, ncid, realtype, dim_id_loc, dim_id_rad, dim_id_time)

  if((noahmp%diag%albedo_direct_snow%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%albedo_direct_snow%output_flag  .and. io_type == output )) &
    call Define2dReal(noahmp%diag%albedo_direct_snow, ncid, realtype, dim_id_loc, dim_id_rad, dim_id_time)

  if((noahmp%diag%albedo_diffuse_snow%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%albedo_diffuse_snow%output_flag  .and. io_type == output )) &
    call Define2dReal(noahmp%diag%albedo_diffuse_snow, ncid, realtype, dim_id_loc, dim_id_rad, dim_id_time)

  if((noahmp%diag%emissivity_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%emissivity_total%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%emissivity_total, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%canopy_gap_fraction%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%canopy_gap_fraction%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%canopy_gap_fraction, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%incanopy_gap_fraction%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%incanopy_gap_fraction%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%incanopy_gap_fraction, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%precip_frozen_frac%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%precip_frozen_frac%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%precip_frozen_frac, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%snow_cover_fraction%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%snow_cover_fraction%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%snow_cover_fraction, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%canopy_wet_fraction%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%canopy_wet_fraction%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%canopy_wet_fraction, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%canopy_water%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%canopy_water%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%canopy_water, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%depth_water_table%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%depth_water_table%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%depth_water_table, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%lai_sunlit%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%lai_sunlit%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%lai_sunlit, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%lai_shaded%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%lai_shaded%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%lai_shaded, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%snow_ice_frac_old%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%snow_ice_frac_old%output_flag  .and. io_type == output )) &
    call Define2dReal(noahmp%diag%snow_ice_frac_old, ncid, realtype, dim_id_loc, dim_id_snow, dim_id_time)

  if((noahmp%diag%snow_albedo_old%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%snow_albedo_old%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%snow_albedo_old, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%evaporation_potential%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%evaporation_potential%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%evaporation_potential, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%soil_moisture_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%soil_moisture_total%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%soil_moisture_total, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%temperature_veg_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%temperature_veg_2m%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%temperature_veg_2m, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%temperature_bare_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%temperature_bare_2m%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%temperature_bare_2m, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%temperature_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%temperature_2m%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%temperature_2m, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%spec_humidity_veg_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%spec_humidity_veg_2m%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%spec_humidity_veg_2m, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%spec_humidity_bare_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%spec_humidity_bare_2m%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%spec_humidity_bare_2m, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%spec_humidity_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%spec_humidity_2m%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%spec_humidity_2m, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%diag%spec_humidity_surface%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%spec_humidity_surface%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%diag%spec_humidity_surface, ncid, realtype, dim_id_loc, dim_id_time)

! Begin noahmp%state variables

  if((noahmp%state%temperature_soil%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_soil%output_flag  .and. io_type == output )) &
    call Define2dReal(noahmp%state%temperature_soil, ncid, realtype, dim_id_loc, dim_id_soil, dim_id_time)

  if((noahmp%state%temperature_snow%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_snow%output_flag  .and. io_type == output )) &
    call Define2dReal(noahmp%state%temperature_snow, ncid, realtype, dim_id_loc, dim_id_snow, dim_id_time)

  if((noahmp%state%temperature_canopy_air%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_canopy_air%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%temperature_canopy_air, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%temperature_radiative%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_radiative%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%temperature_radiative, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%temperature_leaf%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_leaf%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%temperature_leaf, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%temperature_ground%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_ground%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%temperature_ground, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%temperature_bare_grd%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_bare_grd%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%temperature_bare_grd, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%temperature_veg_grd%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_veg_grd%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%temperature_veg_grd, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%vapor_pres_canopy_air%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%vapor_pres_canopy_air%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%vapor_pres_canopy_air, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%soil_liquid_vol%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%soil_liquid_vol%output_flag  .and. io_type == output )) &
    call Define2dReal(noahmp%state%soil_liquid_vol, ncid, realtype, dim_id_loc, dim_id_soil, dim_id_time)

  if((noahmp%state%soil_moisture_vol%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%soil_moisture_vol%output_flag  .and. io_type == output )) &
    call Define2dReal(noahmp%state%soil_moisture_vol, ncid, realtype, dim_id_loc, dim_id_soil, dim_id_time)

  if((noahmp%state%snow_water_equiv%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%snow_water_equiv%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%snow_water_equiv, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%snow_level_ice%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%snow_level_ice%output_flag  .and. io_type == output )) &
    call Define2dReal(noahmp%state%snow_level_ice, ncid, realtype, dim_id_loc, dim_id_snow, dim_id_time)

  if((noahmp%state%snow_level_liquid%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%snow_level_liquid%output_flag  .and. io_type == output )) &
    call Define2dReal(noahmp%state%snow_level_liquid, ncid, realtype, dim_id_loc, dim_id_snow, dim_id_time)

  if((noahmp%state%canopy_liquid%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%canopy_liquid%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%canopy_liquid, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%canopy_ice%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%canopy_ice%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%canopy_ice, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%aquifer_water%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%aquifer_water%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%aquifer_water, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%saturated_water%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%saturated_water%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%saturated_water, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%lake_water%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%lake_water%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%lake_water, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%soil_moisture_wtd%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%soil_moisture_wtd%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%soil_moisture_wtd, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%eq_soil_water_vol%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%eq_soil_water_vol%output_flag  .and. io_type == output )) &
    call Define2dReal(noahmp%state%eq_soil_water_vol, ncid, realtype, dim_id_loc, dim_id_soil, dim_id_time)

  if((noahmp%state%leaf_carbon%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%leaf_carbon%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%leaf_carbon, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%root_carbon%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%root_carbon%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%root_carbon, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%stem_carbon%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%stem_carbon%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%stem_carbon, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%wood_carbon%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%wood_carbon%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%wood_carbon, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%soil_carbon_stable%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%soil_carbon_stable%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%soil_carbon_stable, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%soil_carbon_fast%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%soil_carbon_fast%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%soil_carbon_fast, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%grain_carbon%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%grain_carbon%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%grain_carbon, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%foliage_nitrogen%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%foliage_nitrogen%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%foliage_nitrogen, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%snow_water_equiv_old%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%snow_water_equiv_old%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%snow_water_equiv_old, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%snow_depth%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%snow_depth%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%snow_depth, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%state%snow_age%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%snow_age%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%state%snow_age, ncid, realtype, dim_id_loc, dim_id_time)

! Begin noahmp%flux variables

  if((noahmp%flux%sw_absorbed_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sw_absorbed_total%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%sw_absorbed_total, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%sw_reflected_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sw_reflected_total%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%sw_reflected_total, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%lw_absorbed_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%lw_absorbed_total%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%lw_absorbed_total, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%sensible_heat_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sensible_heat_total%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%sensible_heat_total, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%transpiration_heat%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%transpiration_heat%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%transpiration_heat, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%latent_heat_canopy%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%latent_heat_canopy%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%latent_heat_canopy, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%latent_heat_ground%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%latent_heat_ground%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%latent_heat_ground, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%latent_heat_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%latent_heat_total%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%latent_heat_total, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%ground_heat_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%ground_heat_total%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%ground_heat_total, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%precip_adv_heat_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%precip_adv_heat_total%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%precip_adv_heat_total, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%sw_absorbed_veg%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sw_absorbed_veg%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%sw_absorbed_veg, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%sw_absorbed_ground%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sw_absorbed_ground%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%sw_absorbed_ground, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%lw_absorbed_grd_veg%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%lw_absorbed_grd_veg%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%lw_absorbed_grd_veg, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%lw_absorbed_leaf%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%lw_absorbed_leaf%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%lw_absorbed_leaf, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%lw_absorbed_grd_bare%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%lw_absorbed_grd_bare%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%lw_absorbed_grd_bare, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%sensible_heat_grd_veg%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sensible_heat_grd_veg%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%sensible_heat_grd_veg, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%sensible_heat_leaf%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sensible_heat_leaf%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%sensible_heat_leaf, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%sensible_heat_grd_bar%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sensible_heat_grd_bar%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%sensible_heat_grd_bar, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%latent_heat_trans%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%latent_heat_trans%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%latent_heat_trans, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%latent_heat_leaf%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%latent_heat_leaf%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%latent_heat_leaf, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%latent_heat_grd_veg%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%latent_heat_grd_veg%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%latent_heat_grd_veg, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%latent_heat_grd_bare%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%latent_heat_grd_bare%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%latent_heat_grd_bare, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%snow_sublimation%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%snow_sublimation%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%snow_sublimation, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%ground_heat_veg%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%ground_heat_veg%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%ground_heat_veg, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%ground_heat_bare%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%ground_heat_bare%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%ground_heat_bare, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%precip_adv_heat_veg%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%precip_adv_heat_veg%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%precip_adv_heat_veg, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%precip_adv_heat_grd_v%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%precip_adv_heat_grd_v%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%precip_adv_heat_grd_v, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%precip_adv_heat_grd_b%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%precip_adv_heat_grd_b%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%precip_adv_heat_grd_b, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%transpiration%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%transpiration%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%transpiration, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%evaporation_canopy%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%evaporation_canopy%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%evaporation_canopy, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%evaporation_soil%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%evaporation_soil%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%evaporation_soil, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%runoff_surface%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%runoff_surface%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%runoff_surface, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%runoff_baseflow%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%runoff_baseflow%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%runoff_baseflow, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%snowmelt_out%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%snowmelt_out%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%snowmelt_out, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%snowmelt_shallow%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%snowmelt_shallow%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%snowmelt_shallow, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%snowmelt_shallow_1%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%snowmelt_shallow_1%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%snowmelt_shallow_1, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%snowmelt_shallow_2%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%snowmelt_shallow_2%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%snowmelt_shallow_2, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%deep_recharge%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%deep_recharge%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%deep_recharge, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%recharge%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%recharge%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%recharge, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%par_absorbed%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%par_absorbed%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%par_absorbed, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%photosynthesis%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%photosynthesis%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%photosynthesis, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%net_eco_exchange%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%net_eco_exchange%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%net_eco_exchange, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%global_prim_prod%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%global_prim_prod%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%global_prim_prod, ncid, realtype, dim_id_loc, dim_id_time)

  if((noahmp%flux%net_prim_prod%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%net_prim_prod%output_flag  .and. io_type == output )) &
    call Define1dReal(noahmp%flux%net_prim_prod, ncid, realtype, dim_id_loc, dim_id_time)

  end subroutine DefineNoahMP

  subroutine WriteNoahMP(io_type, namelist, noahmp, ncid)
  
  use NamelistRead
  use ufsLandNoahMPType
  use ufsLandNetcdf
  
  type(namelist_type)  :: namelist
  type(noahmp_type)    :: noahmp

  integer :: io_type
  integer :: ncid

! Begin noahmp%static variables

  if((noahmp%static%vegetation_category%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%vegetation_category%output_flag  .and. io_type == output )) &
    call Write1dInt(noahmp%static%vegetation_category, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%static%soil_category%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%soil_category%output_flag  .and. io_type == output )) &
    call Write1dInt(noahmp%static%soil_category, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%static%slope_category%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%slope_category%output_flag  .and. io_type == output )) &
    call Write1dInt(noahmp%static%slope_category, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%static%soil_interface_depth%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%soil_interface_depth%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%static%soil_interface_depth, ncid,   &
      start = (/1,1/), count = (/noahmp%static%soil_levels, 1/))

  if((noahmp%static%ice_flag%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%ice_flag%output_flag  .and. io_type == output )) &
    call Write1dInt(noahmp%static%ice_flag, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%static%surface_type%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%surface_type%output_flag  .and. io_type == output )) &
    call Write1dInt(noahmp%static%surface_type, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%static%crop_type%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%crop_type%output_flag  .and. io_type == output )) &
    call Write1dInt(noahmp%static%crop_type, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%static%temperature_soil_bot%restart_flag .and. io_type == restart) .or. &
     (noahmp%static%temperature_soil_bot%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%static%temperature_soil_bot, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%model variables

  if((noahmp%model%latitude%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%latitude%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%latitude, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%cosine_zenith%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%cosine_zenith%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%cosine_zenith, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%forcing_height%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%forcing_height%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%forcing_height, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%vegetation_fraction%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%vegetation_fraction%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%vegetation_fraction, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%max_vegetation_frac%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%max_vegetation_frac%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%max_vegetation_frac, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%snow_levels%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%snow_levels%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%snow_levels, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%interface_depth%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%interface_depth%output_flag  .and. io_type == output )) &
    call Write2dReal(noahmp%model%interface_depth, ncid,   &
      start = (/namelist%subset_start ,                           1, 1/) , &
      count = (/namelist%subset_length, noahmp%static%soil_levels+3, 1/))

  if((noahmp%model%snow_soil_thickness%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%snow_soil_thickness%output_flag  .and. io_type == output )) &
    call Write2dReal(noahmp%model%snow_soil_thickness, ncid,   &
      start = (/namelist%subset_start ,                           1, 1/) , &
      count = (/namelist%subset_length, noahmp%static%soil_levels+3, 1/))

  if((noahmp%model%leaf_area_index%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%leaf_area_index%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%leaf_area_index, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%stem_area_index%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%stem_area_index%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%stem_area_index, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%growing_deg_days%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%growing_deg_days%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%growing_deg_days, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%plant_growth_stage%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%plant_growth_stage%output_flag  .and. io_type == output )) &
    call Write1dInt(noahmp%model%plant_growth_stage, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%cm_noahmp%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%cm_noahmp%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%cm_noahmp, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%ch_noahmp%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%ch_noahmp%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%ch_noahmp, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%ch_vegetated%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%ch_vegetated%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%ch_vegetated, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%ch_bare_ground%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%ch_bare_ground%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%ch_bare_ground, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%ch_leaf%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%ch_leaf%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%ch_leaf, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%ch_below_canopy%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%ch_below_canopy%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%ch_below_canopy, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%ch_vegetated_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%ch_vegetated_2m%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%ch_vegetated_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%ch_bare_ground_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%ch_bare_ground_2m%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%ch_bare_ground_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%friction_velocity%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%friction_velocity%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%friction_velocity, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%rs_sunlit%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%rs_sunlit%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%rs_sunlit, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%rs_shaded%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%rs_shaded%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%rs_shaded, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%leaf_air_resistance%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%leaf_air_resistance%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%leaf_air_resistance, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%pbl_height%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%pbl_height%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%pbl_height, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%mo_length_inverse%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%mo_length_inverse%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%mo_length_inverse, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%heat_flux_multiplier%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%heat_flux_multiplier%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%heat_flux_multiplier, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%model%moisture_flux_multiplier%restart_flag .and. io_type == restart) .or. &
     (noahmp%model%moisture_flux_multiplier%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%model%moisture_flux_multiplier, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%forcing variables

  if((noahmp%forcing%temperature_forcing%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%temperature_forcing%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%forcing%temperature_forcing, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%forcing%specific_humidity_forcing%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%specific_humidity_forcing%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%forcing%specific_humidity_forcing, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%forcing%surface_pressure_forcing%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%surface_pressure_forcing%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%forcing%surface_pressure_forcing, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%forcing%wind_speed_forcing%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%wind_speed_forcing%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%forcing%wind_speed_forcing, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%forcing%downward_longwave_forcing%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%downward_longwave_forcing%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%forcing%downward_longwave_forcing, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%forcing%downward_shortwave_forcing%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%downward_shortwave_forcing%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%forcing%downward_shortwave_forcing, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%forcing%precipitation_forcing%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%precipitation_forcing%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%forcing%precipitation_forcing, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%forcing%precip_convective%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%precip_convective%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%forcing%precip_convective, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%forcing%precip_non_convective%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%precip_non_convective%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%forcing%precip_non_convective, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%forcing%precip_snow%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%precip_snow%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%forcing%precip_snow, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%forcing%precip_graupel%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%precip_graupel%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%forcing%precip_graupel, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%forcing%precip_hail%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%precip_hail%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%forcing%precip_hail, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%forcing%snowfall%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%snowfall%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%forcing%snowfall, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%forcing%rainfall%restart_flag .and. io_type == restart) .or. &
     (noahmp%forcing%rainfall%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%forcing%rainfall, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%diag variables

  if((noahmp%diag%z0_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%z0_total%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%z0_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%z0h_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%z0h_total%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%z0h_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%albedo_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%albedo_total%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%albedo_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%albedo_direct%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%albedo_direct%output_flag  .and. io_type == output )) &
    call Write2dReal(noahmp%diag%albedo_direct, ncid,   &
      start = (/namelist%subset_start , 1, 1/) , &
      count = (/namelist%subset_length, 2, 1/))

  if((noahmp%diag%albedo_diffuse%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%albedo_diffuse%output_flag  .and. io_type == output )) &
    call Write2dReal(noahmp%diag%albedo_diffuse, ncid,   &
      start = (/namelist%subset_start , 1, 1/) , &
      count = (/namelist%subset_length, 2, 1/))

  if((noahmp%diag%albedo_direct_snow%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%albedo_direct_snow%output_flag  .and. io_type == output )) &
    call Write2dReal(noahmp%diag%albedo_direct_snow, ncid,   &
      start = (/namelist%subset_start , 1, 1/) , &
      count = (/namelist%subset_length, 2, 1/))

  if((noahmp%diag%albedo_diffuse_snow%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%albedo_diffuse_snow%output_flag  .and. io_type == output )) &
    call Write2dReal(noahmp%diag%albedo_diffuse_snow, ncid,   &
      start = (/namelist%subset_start , 1, 1/) , &
      count = (/namelist%subset_length, 2, 1/))

  if((noahmp%diag%emissivity_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%emissivity_total%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%emissivity_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%canopy_gap_fraction%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%canopy_gap_fraction%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%canopy_gap_fraction, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%incanopy_gap_fraction%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%incanopy_gap_fraction%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%incanopy_gap_fraction, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%precip_frozen_frac%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%precip_frozen_frac%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%precip_frozen_frac, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%snow_cover_fraction%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%snow_cover_fraction%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%snow_cover_fraction, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%canopy_wet_fraction%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%canopy_wet_fraction%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%canopy_wet_fraction, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%canopy_water%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%canopy_water%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%canopy_water, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%depth_water_table%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%depth_water_table%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%depth_water_table, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%lai_sunlit%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%lai_sunlit%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%lai_sunlit, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%lai_shaded%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%lai_shaded%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%lai_shaded, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%snow_ice_frac_old%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%snow_ice_frac_old%output_flag  .and. io_type == output )) &
    call Write2dReal(noahmp%diag%snow_ice_frac_old, ncid,   &
      start = (/namelist%subset_start , 1, 1/) , &
      count = (/namelist%subset_length, 3, 1/))

  if((noahmp%diag%snow_albedo_old%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%snow_albedo_old%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%snow_albedo_old, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%evaporation_potential%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%evaporation_potential%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%evaporation_potential, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%soil_moisture_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%soil_moisture_total%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%soil_moisture_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%temperature_veg_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%temperature_veg_2m%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%temperature_veg_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%temperature_bare_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%temperature_bare_2m%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%temperature_bare_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%temperature_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%temperature_2m%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%temperature_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%spec_humidity_veg_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%spec_humidity_veg_2m%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%spec_humidity_veg_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%spec_humidity_bare_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%spec_humidity_bare_2m%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%spec_humidity_bare_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%spec_humidity_2m%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%spec_humidity_2m%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%spec_humidity_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%diag%spec_humidity_surface%restart_flag .and. io_type == restart) .or. &
     (noahmp%diag%spec_humidity_surface%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%diag%spec_humidity_surface, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%state variables

  if((noahmp%state%temperature_soil%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_soil%output_flag  .and. io_type == output )) &
    call Write2dReal(noahmp%state%temperature_soil, ncid,    &
      start = (/namelist%subset_start , 1, 1/) ,             &
      count = (/namelist%subset_length, noahmp%static%soil_levels, 1/))

  if((noahmp%state%temperature_snow%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_snow%output_flag  .and. io_type == output )) &
    call Write2dReal(noahmp%state%temperature_snow, ncid,   &
      start = (/namelist%subset_start , 1, 1/) , &
      count = (/namelist%subset_length, 3, 1/))

  if((noahmp%state%temperature_canopy_air%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_canopy_air%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%temperature_canopy_air, ncid,    &
     start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%temperature_radiative%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_radiative%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%temperature_radiative, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%temperature_leaf%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_leaf%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%temperature_leaf, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%temperature_ground%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_ground%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%temperature_ground, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%temperature_bare_grd%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_bare_grd%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%temperature_bare_grd, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%temperature_veg_grd%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%temperature_veg_grd%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%temperature_veg_grd, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%vapor_pres_canopy_air%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%vapor_pres_canopy_air%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%vapor_pres_canopy_air, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%soil_liquid_vol%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%soil_liquid_vol%output_flag  .and. io_type == output )) &
    call Write2dReal(noahmp%state%soil_liquid_vol, ncid,     &
      start = (/namelist%subset_start , 1, 1/) ,             &
      count = (/namelist%subset_length, noahmp%static%soil_levels, 1/))

  if((noahmp%state%soil_moisture_vol%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%soil_moisture_vol%output_flag  .and. io_type == output )) &
    call Write2dReal(noahmp%state%soil_moisture_vol, ncid,   &
      start = (/namelist%subset_start , 1, 1/) ,             &
      count = (/namelist%subset_length, noahmp%static%soil_levels, 1/))

  if((noahmp%state%snow_water_equiv%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%snow_water_equiv%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%snow_water_equiv, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%snow_level_ice%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%snow_level_ice%output_flag  .and. io_type == output )) &
    call Write2dReal(noahmp%state%snow_level_ice, ncid,   &
      start = (/namelist%subset_start ,                           1, 1/) , &
      count = (/namelist%subset_length,                           3, 1/))

  if((noahmp%state%snow_level_liquid%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%snow_level_liquid%output_flag  .and. io_type == output )) &
    call Write2dReal(noahmp%state%snow_level_liquid, ncid,   &
      start = (/namelist%subset_start ,                           1, 1/) , &
      count = (/namelist%subset_length,                           3, 1/))

  if((noahmp%state%canopy_liquid%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%canopy_liquid%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%canopy_liquid, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%canopy_ice%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%canopy_ice%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%canopy_ice, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%aquifer_water%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%aquifer_water%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%aquifer_water, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%saturated_water%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%saturated_water%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%saturated_water, ncid,    &
     start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%lake_water%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%lake_water%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%lake_water, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%soil_moisture_wtd%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%soil_moisture_wtd%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%soil_moisture_wtd, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%eq_soil_water_vol%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%eq_soil_water_vol%output_flag  .and. io_type == output )) &
    call Write2dReal(noahmp%state%eq_soil_water_vol, ncid,   &
      start = (/namelist%subset_start ,                           1, 1/) , &
      count = (/namelist%subset_length, noahmp%static%soil_levels  , 1/))

  if((noahmp%state%leaf_carbon%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%leaf_carbon%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%leaf_carbon, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%root_carbon%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%root_carbon%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%root_carbon, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%stem_carbon%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%stem_carbon%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%stem_carbon, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%wood_carbon%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%wood_carbon%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%wood_carbon, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%soil_carbon_stable%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%soil_carbon_stable%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%soil_carbon_stable, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%soil_carbon_fast%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%soil_carbon_fast%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%soil_carbon_fast, ncid,    &
     start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%grain_carbon%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%grain_carbon%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%grain_carbon, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%foliage_nitrogen%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%foliage_nitrogen%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%foliage_nitrogen, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%snow_water_equiv_old%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%snow_water_equiv_old%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%snow_water_equiv_old, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%snow_depth%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%snow_depth%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%snow_depth, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%state%snow_age%restart_flag .and. io_type == restart) .or. &
     (noahmp%state%snow_age%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%state%snow_age, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%flux variables

  if((noahmp%flux%sw_absorbed_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sw_absorbed_total%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%sw_absorbed_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%sw_reflected_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sw_reflected_total%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%sw_reflected_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%lw_absorbed_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%lw_absorbed_total%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%lw_absorbed_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%sensible_heat_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sensible_heat_total%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%sensible_heat_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%transpiration_heat%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%transpiration_heat%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%transpiration_heat, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%latent_heat_canopy%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%latent_heat_canopy%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%latent_heat_canopy, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%latent_heat_ground%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%latent_heat_ground%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%latent_heat_ground, ncid,    &
     start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%latent_heat_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%latent_heat_total%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%latent_heat_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%ground_heat_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%ground_heat_total%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%ground_heat_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%precip_adv_heat_total%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%precip_adv_heat_total%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%precip_adv_heat_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%sw_absorbed_veg%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sw_absorbed_veg%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%sw_absorbed_veg, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%sw_absorbed_ground%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sw_absorbed_ground%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%sw_absorbed_ground, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%lw_absorbed_grd_veg%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%lw_absorbed_grd_veg%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%lw_absorbed_grd_veg, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%lw_absorbed_leaf%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%lw_absorbed_leaf%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%lw_absorbed_leaf, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%lw_absorbed_grd_bare%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%lw_absorbed_grd_bare%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%lw_absorbed_grd_bare, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%sensible_heat_grd_veg%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sensible_heat_grd_veg%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%sensible_heat_grd_veg, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%sensible_heat_leaf%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sensible_heat_leaf%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%sensible_heat_leaf, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%sensible_heat_grd_bar%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%sensible_heat_grd_bar%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%sensible_heat_grd_bar, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%latent_heat_trans%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%latent_heat_trans%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%latent_heat_trans, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%latent_heat_leaf%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%latent_heat_leaf%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%latent_heat_leaf, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%latent_heat_grd_veg%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%latent_heat_grd_veg%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%latent_heat_grd_veg, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%latent_heat_grd_bare%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%latent_heat_grd_bare%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%latent_heat_grd_bare, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%snow_sublimation%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%snow_sublimation%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%snow_sublimation, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%ground_heat_veg%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%ground_heat_veg%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%ground_heat_veg, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%ground_heat_bare%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%ground_heat_bare%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%ground_heat_bare, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%precip_adv_heat_veg%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%precip_adv_heat_veg%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%precip_adv_heat_veg, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%precip_adv_heat_grd_v%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%precip_adv_heat_grd_v%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%precip_adv_heat_grd_v, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%precip_adv_heat_grd_b%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%precip_adv_heat_grd_b%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%precip_adv_heat_grd_b, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%transpiration%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%transpiration%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%transpiration, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%evaporation_canopy%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%evaporation_canopy%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%evaporation_canopy, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%evaporation_soil%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%evaporation_soil%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%evaporation_soil, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%runoff_surface%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%runoff_surface%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%runoff_surface, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%runoff_baseflow%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%runoff_baseflow%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%runoff_baseflow, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%snowmelt_out%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%snowmelt_out%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%snowmelt_out, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%snowmelt_shallow%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%snowmelt_shallow%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%snowmelt_shallow, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%snowmelt_shallow_1%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%snowmelt_shallow_1%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%snowmelt_shallow_1, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%snowmelt_shallow_2%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%snowmelt_shallow_2%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%snowmelt_shallow_2, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%deep_recharge%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%deep_recharge%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%deep_recharge, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%recharge%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%recharge%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%recharge, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%par_absorbed%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%par_absorbed%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%par_absorbed, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%photosynthesis%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%photosynthesis%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%photosynthesis, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%net_eco_exchange%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%net_eco_exchange%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%net_eco_exchange, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%global_prim_prod%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%global_prim_prod%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%global_prim_prod, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if((noahmp%flux%net_prim_prod%restart_flag .and. io_type == restart) .or. &
     (noahmp%flux%net_prim_prod%output_flag  .and. io_type == output )) &
    call Write1dReal(noahmp%flux%net_prim_prod, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  end subroutine WriteNoahMP

  subroutine ReadNoahMP(namelist, noahmp, ncid)
  
  use NamelistRead
  use ufsLandNoahMPType
  use ufsLandNetcdf
  
  type(namelist_type)  :: namelist
  type(noahmp_type)    :: noahmp

  integer :: ncid

! Begin noahmp%static variables

  if(noahmp%static%vegetation_category%restart_flag) &
    call Read1dInt(noahmp%static%vegetation_category, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%static%soil_category%restart_flag) &
    call Read1dInt(noahmp%static%soil_category, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%static%slope_category%restart_flag) &
    call Read1dInt(noahmp%static%slope_category, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%static%soil_interface_depth%restart_flag) &
    call Read1dReal(noahmp%static%soil_interface_depth, ncid,   &
      start = (/1,1/), count = (/noahmp%static%soil_levels, 1/))

  if(noahmp%static%ice_flag%restart_flag) &
    call Read1dInt(noahmp%static%ice_flag, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%static%surface_type%restart_flag) &
    call Read1dInt(noahmp%static%surface_type, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%static%crop_type%restart_flag) &
    call Read1dInt(noahmp%static%crop_type, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%static%temperature_soil_bot%restart_flag) &
    call Read1dReal(noahmp%static%temperature_soil_bot, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%model variables

  if(noahmp%model%latitude%restart_flag) &
    call Read1dReal(noahmp%model%latitude, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%cosine_zenith%restart_flag) &
    call Read1dReal(noahmp%model%cosine_zenith, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%forcing_height%restart_flag) &
    call Read1dReal(noahmp%model%forcing_height, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%vegetation_fraction%restart_flag) &
    call Read1dReal(noahmp%model%vegetation_fraction, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%max_vegetation_frac%restart_flag) &
    call Read1dReal(noahmp%model%max_vegetation_frac, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%snow_levels%restart_flag) &
    call Read1dReal(noahmp%model%snow_levels, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%interface_depth%restart_flag) &
    call Read2dReal(noahmp%model%interface_depth, ncid,   &
      start = (/namelist%subset_start ,                           1, 1/) , &
      count = (/namelist%subset_length, noahmp%static%soil_levels+3, 1/))

  if(noahmp%model%snow_soil_thickness%restart_flag) &
    call Read2dReal(noahmp%model%snow_soil_thickness, ncid,   &
      start = (/namelist%subset_start ,                           1, 1/) , &
      count = (/namelist%subset_length, noahmp%static%soil_levels+3, 1/))

  if(noahmp%model%leaf_area_index%restart_flag) &
    call Read1dReal(noahmp%model%leaf_area_index, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%stem_area_index%restart_flag) &
    call Read1dReal(noahmp%model%stem_area_index, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%growing_deg_days%restart_flag) &
    call Read1dReal(noahmp%model%growing_deg_days, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%plant_growth_stage%restart_flag) &
    call Read1dInt(noahmp%model%plant_growth_stage, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%cm_noahmp%restart_flag) &
    call Read1dReal(noahmp%model%cm_noahmp, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%ch_noahmp%restart_flag) &
    call Read1dReal(noahmp%model%ch_noahmp, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%ch_vegetated%restart_flag) &
    call Read1dReal(noahmp%model%ch_vegetated, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%ch_bare_ground%restart_flag) &
    call Read1dReal(noahmp%model%ch_bare_ground, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%ch_leaf%restart_flag) &
    call Read1dReal(noahmp%model%ch_leaf, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%ch_below_canopy%restart_flag) &
    call Read1dReal(noahmp%model%ch_below_canopy, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%ch_vegetated_2m%restart_flag) &
    call Read1dReal(noahmp%model%ch_vegetated_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%ch_bare_ground_2m%restart_flag) &
    call Read1dReal(noahmp%model%ch_bare_ground_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%friction_velocity%restart_flag) &
    call Read1dReal(noahmp%model%friction_velocity, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%rs_sunlit%restart_flag) &
    call Read1dReal(noahmp%model%rs_sunlit, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%rs_shaded%restart_flag) &
    call Read1dReal(noahmp%model%rs_shaded, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%leaf_air_resistance%restart_flag) &
    call Read1dReal(noahmp%model%leaf_air_resistance, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%pbl_height%restart_flag) &
    call Read1dReal(noahmp%model%pbl_height, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%mo_length_inverse%restart_flag) &
    call Read1dReal(noahmp%model%mo_length_inverse, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%heat_flux_multiplier%restart_flag) &
    call Read1dReal(noahmp%model%heat_flux_multiplier, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%moisture_flux_multiplier%restart_flag) &
    call Read1dReal(noahmp%model%moisture_flux_multiplier, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%forcing variables

  if(noahmp%forcing%temperature_forcing%restart_flag) &
    call Read1dReal(noahmp%forcing%temperature_forcing, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%specific_humidity_forcing%restart_flag) &
    call Read1dReal(noahmp%forcing%specific_humidity_forcing, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%surface_pressure_forcing%restart_flag) &
    call Read1dReal(noahmp%forcing%surface_pressure_forcing, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%wind_speed_forcing%restart_flag) &
    call Read1dReal(noahmp%forcing%wind_speed_forcing, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%downward_longwave_forcing%restart_flag) &
    call Read1dReal(noahmp%forcing%downward_longwave_forcing, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%downward_shortwave_forcing%restart_flag) &
    call Read1dReal(noahmp%forcing%downward_shortwave_forcing, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%precipitation_forcing%restart_flag) &
    call Read1dReal(noahmp%forcing%precipitation_forcing, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%precip_convective%restart_flag) &
    call Read1dReal(noahmp%forcing%precip_convective, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%precip_non_convective%restart_flag) &
    call Read1dReal(noahmp%forcing%precip_non_convective, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%precip_snow%restart_flag) &
    call Read1dReal(noahmp%forcing%precip_snow, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%precip_graupel%restart_flag) &
    call Read1dReal(noahmp%forcing%precip_graupel, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%precip_hail%restart_flag) &
    call Read1dReal(noahmp%forcing%precip_hail, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%snowfall%restart_flag) &
    call Read1dReal(noahmp%forcing%snowfall, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%rainfall%restart_flag) &
    call Read1dReal(noahmp%forcing%rainfall, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%diag variables

  if(noahmp%diag%z0_total%restart_flag) &
    call Read1dReal(noahmp%diag%z0_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%z0h_total%restart_flag) &
    call Read1dReal(noahmp%diag%z0h_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%albedo_total%restart_flag) &
    call Read1dReal(noahmp%diag%albedo_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%albedo_direct%restart_flag) &
    call Read2dReal(noahmp%diag%albedo_direct, ncid,   &
      start = (/namelist%subset_start , 1, 1/) , &
      count = (/namelist%subset_length, 2, 1/))

  if(noahmp%diag%albedo_diffuse%restart_flag) &
    call Read2dReal(noahmp%diag%albedo_diffuse, ncid,   &
      start = (/namelist%subset_start , 1, 1/) , &
      count = (/namelist%subset_length, 2, 1/))

  if(noahmp%diag%albedo_direct_snow%restart_flag) &
    call Read2dReal(noahmp%diag%albedo_direct_snow, ncid,   &
      start = (/namelist%subset_start , 1, 1/) , &
      count = (/namelist%subset_length, 2, 1/))

  if(noahmp%diag%albedo_diffuse_snow%restart_flag) &
    call Read2dReal(noahmp%diag%albedo_diffuse_snow, ncid,   &
      start = (/namelist%subset_start , 1, 1/) , &
      count = (/namelist%subset_length, 2, 1/))

  if(noahmp%diag%emissivity_total%restart_flag) &
    call Read1dReal(noahmp%diag%emissivity_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%canopy_gap_fraction%restart_flag) &
    call Read1dReal(noahmp%diag%canopy_gap_fraction, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%incanopy_gap_fraction%restart_flag) &
    call Read1dReal(noahmp%diag%incanopy_gap_fraction, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%precip_frozen_frac%restart_flag) &
    call Read1dReal(noahmp%diag%precip_frozen_frac, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%snow_cover_fraction%restart_flag) &
    call Read1dReal(noahmp%diag%snow_cover_fraction, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%canopy_wet_fraction%restart_flag) &
    call Read1dReal(noahmp%diag%canopy_wet_fraction, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%canopy_water%restart_flag) &
    call Read1dReal(noahmp%diag%canopy_water, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%depth_water_table%restart_flag) &
    call Read1dReal(noahmp%diag%depth_water_table, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%lai_sunlit%restart_flag) &
    call Read1dReal(noahmp%diag%lai_sunlit, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%lai_shaded%restart_flag) &
    call Read1dReal(noahmp%diag%lai_shaded, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%snow_ice_frac_old%restart_flag) &
    call Read2dReal(noahmp%diag%snow_ice_frac_old, ncid,   &
      start = (/namelist%subset_start , 1, 1/) , &
      count = (/namelist%subset_length, 3, 1/))

  if(noahmp%diag%snow_albedo_old%restart_flag) &
    call Read1dReal(noahmp%diag%snow_albedo_old, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%evaporation_potential%restart_flag) &
    call Read1dReal(noahmp%diag%evaporation_potential, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%soil_moisture_total%restart_flag) &
    call Read1dReal(noahmp%diag%soil_moisture_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%temperature_veg_2m%restart_flag) &
    call Read1dReal(noahmp%diag%temperature_veg_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%temperature_bare_2m%restart_flag) &
    call Read1dReal(noahmp%diag%temperature_bare_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%temperature_2m%restart_flag) &
    call Read1dReal(noahmp%diag%temperature_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%spec_humidity_veg_2m%restart_flag) &
    call Read1dReal(noahmp%diag%spec_humidity_veg_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%spec_humidity_bare_2m%restart_flag) &
    call Read1dReal(noahmp%diag%spec_humidity_bare_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%spec_humidity_2m%restart_flag) &
    call Read1dReal(noahmp%diag%spec_humidity_2m, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%spec_humidity_surface%restart_flag) &
    call Read1dReal(noahmp%diag%spec_humidity_surface, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%state variables

  if(noahmp%state%temperature_soil%restart_flag) &
    call Read2dReal(noahmp%state%temperature_soil, ncid,    &
      start = (/namelist%subset_start , 1, 1/) ,             &
      count = (/namelist%subset_length, noahmp%static%soil_levels, 1/))

  if(noahmp%state%temperature_snow%restart_flag) &
    call Read2dReal(noahmp%state%temperature_snow, ncid,   &
      start = (/namelist%subset_start , 1, 1/) , &
      count = (/namelist%subset_length, 3, 1/))

  if(noahmp%state%temperature_canopy_air%restart_flag) &
    call Read1dReal(noahmp%state%temperature_canopy_air, ncid,    &
     start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%temperature_radiative%restart_flag) &
    call Read1dReal(noahmp%state%temperature_radiative, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%temperature_leaf%restart_flag) &
    call Read1dReal(noahmp%state%temperature_leaf, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%temperature_ground%restart_flag) &
    call Read1dReal(noahmp%state%temperature_ground, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%temperature_bare_grd%restart_flag) &
    call Read1dReal(noahmp%state%temperature_bare_grd, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%temperature_veg_grd%restart_flag) &
    call Read1dReal(noahmp%state%temperature_veg_grd, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%vapor_pres_canopy_air%restart_flag) &
    call Read1dReal(noahmp%state%vapor_pres_canopy_air, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%soil_liquid_vol%restart_flag) &
    call Read2dReal(noahmp%state%soil_liquid_vol, ncid,     &
      start = (/namelist%subset_start , 1, 1/) ,             &
      count = (/namelist%subset_length, noahmp%static%soil_levels, 1/))

  if(noahmp%state%soil_moisture_vol%restart_flag) &
    call Read2dReal(noahmp%state%soil_moisture_vol, ncid,   &
      start = (/namelist%subset_start , 1, 1/) ,             &
      count = (/namelist%subset_length, noahmp%static%soil_levels, 1/))

  if(noahmp%state%snow_water_equiv%restart_flag) &
    call Read1dReal(noahmp%state%snow_water_equiv, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%snow_level_ice%restart_flag) &
    call Read2dReal(noahmp%state%snow_level_ice, ncid,   &
      start = (/namelist%subset_start ,                           1, 1/) , &
      count = (/namelist%subset_length,                           3, 1/))

  if(noahmp%state%snow_level_liquid%restart_flag) &
    call Read2dReal(noahmp%state%snow_level_liquid, ncid,   &
      start = (/namelist%subset_start ,                           1, 1/) , &
      count = (/namelist%subset_length,                           3, 1/))

  if(noahmp%state%canopy_liquid%restart_flag) &
    call Read1dReal(noahmp%state%canopy_liquid, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%canopy_ice%restart_flag) &
    call Read1dReal(noahmp%state%canopy_ice, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%aquifer_water%restart_flag) &
    call Read1dReal(noahmp%state%aquifer_water, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%saturated_water%restart_flag) &
    call Read1dReal(noahmp%state%saturated_water, ncid,    &
     start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%lake_water%restart_flag) &
    call Read1dReal(noahmp%state%lake_water, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%soil_moisture_wtd%restart_flag) &
    call Read1dReal(noahmp%state%soil_moisture_wtd, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%eq_soil_water_vol%restart_flag) &
    call Read2dReal(noahmp%state%eq_soil_water_vol, ncid,   &
      start = (/namelist%subset_start ,                           1, 1/) , &
      count = (/namelist%subset_length, noahmp%static%soil_levels  , 1/))

  if(noahmp%state%leaf_carbon%restart_flag) &
    call Read1dReal(noahmp%state%leaf_carbon, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%root_carbon%restart_flag) &
    call Read1dReal(noahmp%state%root_carbon, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%stem_carbon%restart_flag) &
    call Read1dReal(noahmp%state%stem_carbon, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%wood_carbon%restart_flag) &
    call Read1dReal(noahmp%state%wood_carbon, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%soil_carbon_stable%restart_flag) &
    call Read1dReal(noahmp%state%soil_carbon_stable, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%soil_carbon_fast%restart_flag) &
    call Read1dReal(noahmp%state%soil_carbon_fast, ncid,    &
     start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%grain_carbon%restart_flag) &
    call Read1dReal(noahmp%state%grain_carbon, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%foliage_nitrogen%restart_flag) &
    call Read1dReal(noahmp%state%foliage_nitrogen, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%snow_water_equiv_old%restart_flag) &
    call Read1dReal(noahmp%state%snow_water_equiv_old, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%snow_depth%restart_flag) &
    call Read1dReal(noahmp%state%snow_depth, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%snow_age%restart_flag) &
    call Read1dReal(noahmp%state%snow_age, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%flux variables

  if(noahmp%flux%sw_absorbed_total%restart_flag) &
    call Read1dReal(noahmp%flux%sw_absorbed_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%sw_reflected_total%restart_flag) &
    call Read1dReal(noahmp%flux%sw_reflected_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%lw_absorbed_total%restart_flag) &
    call Read1dReal(noahmp%flux%lw_absorbed_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%sensible_heat_total%restart_flag) &
    call Read1dReal(noahmp%flux%sensible_heat_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%transpiration_heat%restart_flag) &
    call Read1dReal(noahmp%flux%transpiration_heat, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%latent_heat_canopy%restart_flag) &
    call Read1dReal(noahmp%flux%latent_heat_canopy, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%latent_heat_ground%restart_flag) &
    call Read1dReal(noahmp%flux%latent_heat_ground, ncid,    &
     start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%latent_heat_total%restart_flag) &
    call Read1dReal(noahmp%flux%latent_heat_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%ground_heat_total%restart_flag) &
    call Read1dReal(noahmp%flux%ground_heat_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%precip_adv_heat_total%restart_flag) &
    call Read1dReal(noahmp%flux%precip_adv_heat_total, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%sw_absorbed_veg%restart_flag) &
    call Read1dReal(noahmp%flux%sw_absorbed_veg, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%sw_absorbed_ground%restart_flag) &
    call Read1dReal(noahmp%flux%sw_absorbed_ground, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%lw_absorbed_grd_veg%restart_flag) &
    call Read1dReal(noahmp%flux%lw_absorbed_grd_veg, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%lw_absorbed_leaf%restart_flag) &
    call Read1dReal(noahmp%flux%lw_absorbed_leaf, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%lw_absorbed_grd_bare%restart_flag) &
    call Read1dReal(noahmp%flux%lw_absorbed_grd_bare, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%sensible_heat_grd_veg%restart_flag) &
    call Read1dReal(noahmp%flux%sensible_heat_grd_veg, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%sensible_heat_leaf%restart_flag) &
    call Read1dReal(noahmp%flux%sensible_heat_leaf, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%sensible_heat_grd_bar%restart_flag) &
    call Read1dReal(noahmp%flux%sensible_heat_grd_bar, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%latent_heat_trans%restart_flag) &
    call Read1dReal(noahmp%flux%latent_heat_trans, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%latent_heat_leaf%restart_flag) &
    call Read1dReal(noahmp%flux%latent_heat_leaf, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%latent_heat_grd_veg%restart_flag) &
    call Read1dReal(noahmp%flux%latent_heat_grd_veg, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%latent_heat_grd_bare%restart_flag) &
    call Read1dReal(noahmp%flux%latent_heat_grd_bare, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%snow_sublimation%restart_flag) &
    call Read1dReal(noahmp%flux%snow_sublimation, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%ground_heat_veg%restart_flag) &
    call Read1dReal(noahmp%flux%ground_heat_veg, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%ground_heat_bare%restart_flag) &
    call Read1dReal(noahmp%flux%ground_heat_bare, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%precip_adv_heat_veg%restart_flag) &
    call Read1dReal(noahmp%flux%precip_adv_heat_veg, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%precip_adv_heat_grd_v%restart_flag) &
    call Read1dReal(noahmp%flux%precip_adv_heat_grd_v, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%precip_adv_heat_grd_b%restart_flag) &
    call Read1dReal(noahmp%flux%precip_adv_heat_grd_b, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%transpiration%restart_flag) &
    call Read1dReal(noahmp%flux%transpiration, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%evaporation_canopy%restart_flag) &
    call Read1dReal(noahmp%flux%evaporation_canopy, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%evaporation_soil%restart_flag) &
    call Read1dReal(noahmp%flux%evaporation_soil, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%runoff_surface%restart_flag) &
    call Read1dReal(noahmp%flux%runoff_surface, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%runoff_baseflow%restart_flag) &
    call Read1dReal(noahmp%flux%runoff_baseflow, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%snowmelt_out%restart_flag) &
    call Read1dReal(noahmp%flux%snowmelt_out, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%snowmelt_shallow%restart_flag) &
    call Read1dReal(noahmp%flux%snowmelt_shallow, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%snowmelt_shallow_1%restart_flag) &
    call Read1dReal(noahmp%flux%snowmelt_shallow_1, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%snowmelt_shallow_2%restart_flag) &
    call Read1dReal(noahmp%flux%snowmelt_shallow_2, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%deep_recharge%restart_flag) &
    call Read1dReal(noahmp%flux%deep_recharge, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%recharge%restart_flag) &
    call Read1dReal(noahmp%flux%recharge, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%par_absorbed%restart_flag) &
    call Read1dReal(noahmp%flux%par_absorbed, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%photosynthesis%restart_flag) &
    call Read1dReal(noahmp%flux%photosynthesis, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%net_eco_exchange%restart_flag) &
    call Read1dReal(noahmp%flux%net_eco_exchange, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%global_prim_prod%restart_flag) &
    call Read1dReal(noahmp%flux%global_prim_prod, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%net_prim_prod%restart_flag) &
    call Read1dReal(noahmp%flux%net_prim_prod, ncid,   &
      start = (/namelist%subset_start,1/), count = (/namelist%subset_length, 1/))

  end subroutine ReadNoahMP

end module ufsLandGenericIO
