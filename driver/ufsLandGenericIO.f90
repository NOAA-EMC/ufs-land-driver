module ufsLandGenericIO

  implicit none
  save
  
  integer, parameter, private :: output = 1, restart = 2, daily_mean = 3, monthly_mean = 4,  &
                                 solar_noon = 5, diurnal = 6

contains   

  subroutine DefineNoahMP(io_type, noahmp, ncid, &
                          dim_id_time, dim_id_loc, dim_id_soil, &
                          dim_id_snow, dim_id_snso, dim_id_date, dim_id_rad, dim_id_hour)
  
  use netcdf, only : NF90_DOUBLE, NF90_FLOAT, NF90_INT
  use ufsLandNoahMPType
  use ufsLandNetcdf
  
  type(noahmp_type)    :: noahmp

  integer :: io_type, realtype
  integer :: ncid
  integer :: dim_id_time, dim_id_loc, dim_id_soil, dim_id_snow, dim_id_snso, dim_id_date, dim_id_rad
  integer, intent(in), optional :: dim_id_hour
  
  io_setup : select case(io_type)
  
    case(output, daily_mean, monthly_mean, solar_noon, diurnal)
    
      realtype = NF90_FLOAT    ! write output as single precision

    case(restart)
    
      realtype = NF90_DOUBLE   ! write restart as double precision

    case default

      stop "io_type out of range in DefineNoahMP"

  end select io_setup

! Begin noahmp%static variables

  if((noahmp%static%vegetation_category%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%vegetation_category%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%vegetation_category%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%vegetation_category%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%vegetation_category%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%vegetation_category%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dInt(noahmp%static%vegetation_category, ncid, NF90_INT, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%static%soil_category%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%soil_category%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%soil_category%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%soil_category%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%soil_category%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%soil_category%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dInt(noahmp%static%soil_category, ncid, NF90_INT, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%static%slope_category%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%slope_category%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%slope_category%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%slope_category%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%slope_category%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%slope_category%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dInt(noahmp%static%slope_category, ncid, NF90_INT, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%static%soil_color_category%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%soil_color_category%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%soil_color_category%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%soil_color_category%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%soil_color_category%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%soil_color_category%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dInt(noahmp%static%soil_color_category, ncid, NF90_INT, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%static%soil_interface_depth%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%soil_interface_depth%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%soil_interface_depth%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%soil_interface_depth%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%soil_interface_depth%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%soil_interface_depth%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%static%soil_interface_depth, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%static%ice_flag%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%ice_flag%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%ice_flag%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%ice_flag%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%ice_flag%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%ice_flag%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dInt(noahmp%static%ice_flag, ncid, NF90_INT, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%static%surface_type%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%surface_type%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%surface_type%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%surface_type%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%surface_type%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%surface_type%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dInt(noahmp%static%surface_type, ncid, NF90_INT, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%static%crop_type%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%crop_type%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%crop_type%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%crop_type%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%crop_type%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%crop_type%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dInt(noahmp%static%crop_type, ncid, NF90_INT, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%static%temperature_soil_bot%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%temperature_soil_bot%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%temperature_soil_bot%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%temperature_soil_bot%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%temperature_soil_bot%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%temperature_soil_bot%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%static%temperature_soil_bot, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

! Begin noahmp%model variables

  if((noahmp%model%latitude%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%latitude%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%latitude%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%latitude%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%latitude%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%latitude%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%latitude, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%longitude%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%longitude%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%longitude%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%longitude%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%longitude%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%longitude%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%longitude, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%solar_noon_hour%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%solar_noon_hour%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%solar_noon_hour%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%solar_noon_hour%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%solar_noon_hour%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%solar_noon_hour%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dInt(noahmp%model%solar_noon_hour, ncid, NF90_INT, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%cosine_zenith%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%cosine_zenith%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%cosine_zenith%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%cosine_zenith%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%cosine_zenith%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%cosine_zenith%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%cosine_zenith, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%forcing_height%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%forcing_height%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%forcing_height%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%forcing_height%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%forcing_height%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%forcing_height%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%forcing_height, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%vegetation_fraction%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%vegetation_fraction%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%vegetation_fraction%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%vegetation_fraction%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%vegetation_fraction%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%vegetation_fraction%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%vegetation_fraction, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%max_vegetation_frac%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%max_vegetation_frac%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%max_vegetation_frac%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%max_vegetation_frac%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%max_vegetation_frac%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%max_vegetation_frac%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%max_vegetation_frac, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%active_snow_levels%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%active_snow_levels%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%active_snow_levels%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%active_snow_levels%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%active_snow_levels%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%active_snow_levels%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%active_snow_levels, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%interface_depth%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%interface_depth%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%interface_depth%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%interface_depth%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%interface_depth%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%interface_depth%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define2dReal(noahmp%model%interface_depth, ncid, realtype, dim_id_loc, dim_id_snso, dim_id_time, dim_id_hour)

  if((noahmp%model%snow_soil_thickness%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%snow_soil_thickness%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%snow_soil_thickness%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%snow_soil_thickness%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%snow_soil_thickness%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%snow_soil_thickness%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define2dReal(noahmp%model%snow_soil_thickness, ncid, realtype, dim_id_loc, dim_id_snso, dim_id_time, dim_id_hour)

  if((noahmp%model%leaf_area_index%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%leaf_area_index%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%leaf_area_index%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%leaf_area_index%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%leaf_area_index%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%leaf_area_index%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%leaf_area_index, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%stem_area_index%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%stem_area_index%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%stem_area_index%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%stem_area_index%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%stem_area_index%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%stem_area_index%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%stem_area_index, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%growing_deg_days%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%growing_deg_days%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%growing_deg_days%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%growing_deg_days%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%growing_deg_days%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%growing_deg_days%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%growing_deg_days, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%plant_growth_stage%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%plant_growth_stage%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%plant_growth_stage%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%plant_growth_stage%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%plant_growth_stage%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%plant_growth_stage%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dInt(noahmp%model%plant_growth_stage, ncid, NF90_INT, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%cm_noahmp%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%cm_noahmp%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%cm_noahmp%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%cm_noahmp%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%cm_noahmp%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%cm_noahmp%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%cm_noahmp, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%ch_noahmp%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%ch_noahmp%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%ch_noahmp%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%ch_noahmp%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%ch_noahmp%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%ch_noahmp%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%ch_noahmp, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%ch_vegetated%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%ch_vegetated%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%ch_vegetated%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%ch_vegetated%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%ch_vegetated%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%ch_vegetated%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%ch_vegetated, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%ch_bare_ground%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%ch_bare_ground%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%ch_bare_ground%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%ch_bare_ground%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%ch_bare_ground%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%ch_bare_ground%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%ch_bare_ground, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%ch_leaf%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%ch_leaf%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%ch_leaf%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%ch_leaf%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%ch_leaf%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%ch_leaf%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%ch_leaf, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%ch_below_canopy%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%ch_below_canopy%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%ch_below_canopy%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%ch_below_canopy%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%ch_below_canopy%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%ch_below_canopy%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%ch_below_canopy, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%ch_vegetated_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%ch_vegetated_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%ch_vegetated_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%ch_vegetated_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%ch_vegetated_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%ch_vegetated_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%ch_vegetated_2m, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%ch_bare_ground_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%ch_bare_ground_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%ch_bare_ground_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%ch_bare_ground_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%ch_bare_ground_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%ch_bare_ground_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%ch_bare_ground_2m, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%friction_velocity%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%friction_velocity%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%friction_velocity%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%friction_velocity%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%friction_velocity%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%friction_velocity%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%friction_velocity, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%rs_sunlit%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%rs_sunlit%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%rs_sunlit%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%rs_sunlit%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%rs_sunlit%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%rs_sunlit%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%rs_sunlit, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%rs_shaded%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%rs_shaded%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%rs_shaded%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%rs_shaded%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%rs_shaded%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%rs_shaded%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%rs_shaded, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%leaf_air_resistance%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%leaf_air_resistance%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%leaf_air_resistance%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%leaf_air_resistance%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%leaf_air_resistance%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%leaf_air_resistance%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%leaf_air_resistance, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%pbl_height%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%pbl_height%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%pbl_height%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%pbl_height%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%pbl_height%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%pbl_height%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%pbl_height, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%mo_length_inverse%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%mo_length_inverse%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%mo_length_inverse%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%mo_length_inverse%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%mo_length_inverse%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%mo_length_inverse%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%mo_length_inverse, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%heat_flux_multiplier%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%heat_flux_multiplier%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%heat_flux_multiplier%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%heat_flux_multiplier%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%heat_flux_multiplier%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%heat_flux_multiplier%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%heat_flux_multiplier, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%model%moisture_flux_multiplier%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%moisture_flux_multiplier%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%moisture_flux_multiplier%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%moisture_flux_multiplier%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%moisture_flux_multiplier%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%moisture_flux_multiplier%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%model%moisture_flux_multiplier, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

! Begin noahmp%forcing variables

  if((noahmp%forcing%temperature_forcing%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%temperature_forcing%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%temperature_forcing%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%temperature_forcing%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%temperature_forcing%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%temperature_forcing%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%forcing%temperature_forcing, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%forcing%specific_humidity_forcing%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%specific_humidity_forcing%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%specific_humidity_forcing%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%specific_humidity_forcing%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%specific_humidity_forcing%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%specific_humidity_forcing%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%forcing%specific_humidity_forcing, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%forcing%surface_pressure_forcing%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%surface_pressure_forcing%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%surface_pressure_forcing%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%surface_pressure_forcing%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%surface_pressure_forcing%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%surface_pressure_forcing%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%forcing%surface_pressure_forcing, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%forcing%wind_speed_forcing%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%wind_speed_forcing%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%wind_speed_forcing%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%wind_speed_forcing%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%wind_speed_forcing%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%wind_speed_forcing%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%forcing%wind_speed_forcing, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%forcing%downward_longwave_forcing%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%downward_longwave_forcing%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%downward_longwave_forcing%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%downward_longwave_forcing%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%downward_longwave_forcing%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%downward_longwave_forcing%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%forcing%downward_longwave_forcing, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%forcing%downward_shortwave_forcing%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%downward_shortwave_forcing%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%downward_shortwave_forcing%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%downward_shortwave_forcing%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%downward_shortwave_forcing%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%downward_shortwave_forcing%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%forcing%downward_shortwave_forcing, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%forcing%precipitation_forcing%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%precipitation_forcing%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%precipitation_forcing%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%precipitation_forcing%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%precipitation_forcing%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%precipitation_forcing%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%forcing%precipitation_forcing, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%forcing%precip_convective%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%precip_convective%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%precip_convective%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%precip_convective%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%precip_convective%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%precip_convective%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%forcing%precip_convective, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%forcing%precip_non_convective%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%precip_non_convective%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%precip_non_convective%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%precip_non_convective%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%precip_non_convective%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%precip_non_convective%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%forcing%precip_non_convective, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%forcing%precip_snow%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%precip_snow%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%precip_snow%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%precip_snow%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%precip_snow%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%precip_snow%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%forcing%precip_snow, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%forcing%precip_graupel%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%precip_graupel%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%precip_graupel%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%precip_graupel%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%precip_graupel%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%precip_graupel%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%forcing%precip_graupel, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%forcing%precip_hail%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%precip_hail%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%precip_hail%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%precip_hail%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%precip_hail%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%precip_hail%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%forcing%precip_hail, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%forcing%snowfall%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%snowfall%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%snowfall%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%snowfall%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%snowfall%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%snowfall%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%forcing%snowfall, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%forcing%rainfall%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%rainfall%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%rainfall%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%rainfall%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%rainfall%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%rainfall%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%forcing%rainfall, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

! Begin noahmp%diag variables

  if((noahmp%diag%z0_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%z0_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%z0_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%z0_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%z0_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%z0_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%z0_total, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%z0h_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%z0h_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%z0h_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%z0h_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%z0h_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%z0h_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%z0h_total, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%albedo_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%albedo_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%albedo_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%albedo_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%albedo_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%albedo_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%albedo_total, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%albedo_direct%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%albedo_direct%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%albedo_direct%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%albedo_direct%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%albedo_direct%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%albedo_direct%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define2dReal(noahmp%diag%albedo_direct, ncid, realtype, dim_id_loc, dim_id_rad, dim_id_time, dim_id_hour)

  if((noahmp%diag%albedo_diffuse%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%albedo_diffuse%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%albedo_diffuse%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%albedo_diffuse%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%albedo_diffuse%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%albedo_diffuse%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define2dReal(noahmp%diag%albedo_diffuse, ncid, realtype, dim_id_loc, dim_id_rad, dim_id_time, dim_id_hour)

  if((noahmp%diag%albedo_direct_snow%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%albedo_direct_snow%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%albedo_direct_snow%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%albedo_direct_snow%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%albedo_direct_snow%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%albedo_direct_snow%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define2dReal(noahmp%diag%albedo_direct_snow, ncid, realtype, dim_id_loc, dim_id_rad, dim_id_time, dim_id_hour)

  if((noahmp%diag%albedo_diffuse_snow%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%albedo_diffuse_snow%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%albedo_diffuse_snow%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%albedo_diffuse_snow%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%albedo_diffuse_snow%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%albedo_diffuse_snow%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define2dReal(noahmp%diag%albedo_diffuse_snow, ncid, realtype, dim_id_loc, dim_id_rad, dim_id_time, dim_id_hour)

  if((noahmp%diag%emissivity_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%emissivity_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%emissivity_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%emissivity_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%emissivity_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%emissivity_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%emissivity_total, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%canopy_gap_fraction%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%canopy_gap_fraction%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%canopy_gap_fraction%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%canopy_gap_fraction%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%canopy_gap_fraction%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%canopy_gap_fraction%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%canopy_gap_fraction, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%incanopy_gap_fraction%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%incanopy_gap_fraction%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%incanopy_gap_fraction%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%incanopy_gap_fraction%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%incanopy_gap_fraction%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%incanopy_gap_fraction%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%incanopy_gap_fraction, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%precip_frozen_frac%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%precip_frozen_frac%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%precip_frozen_frac%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%precip_frozen_frac%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%precip_frozen_frac%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%precip_frozen_frac%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%precip_frozen_frac, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%snow_cover_fraction%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%snow_cover_fraction%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%snow_cover_fraction%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%snow_cover_fraction%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%snow_cover_fraction%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%snow_cover_fraction%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%snow_cover_fraction, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%canopy_wet_fraction%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%canopy_wet_fraction%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%canopy_wet_fraction%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%canopy_wet_fraction%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%canopy_wet_fraction%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%canopy_wet_fraction%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%canopy_wet_fraction, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%canopy_water%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%canopy_water%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%canopy_water%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%canopy_water%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%canopy_water%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%canopy_water%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%canopy_water, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%depth_water_table%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%depth_water_table%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%depth_water_table%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%depth_water_table%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%depth_water_table%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%depth_water_table%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%depth_water_table, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%lai_sunlit%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%lai_sunlit%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%lai_sunlit%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%lai_sunlit%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%lai_sunlit%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%lai_sunlit%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%lai_sunlit, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%lai_shaded%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%lai_shaded%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%lai_shaded%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%lai_shaded%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%lai_shaded%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%lai_shaded%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%lai_shaded, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%snow_ice_frac_old%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%snow_ice_frac_old%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%snow_ice_frac_old%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%snow_ice_frac_old%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%snow_ice_frac_old%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%snow_ice_frac_old%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define2dReal(noahmp%diag%snow_ice_frac_old, ncid, realtype, dim_id_loc, dim_id_snow, dim_id_time, dim_id_hour)

  if((noahmp%diag%snow_albedo_old%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%snow_albedo_old%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%snow_albedo_old%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%snow_albedo_old%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%snow_albedo_old%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%snow_albedo_old%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%snow_albedo_old, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%evaporation_potential%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%evaporation_potential%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%evaporation_potential%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%evaporation_potential%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%evaporation_potential%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%evaporation_potential%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%evaporation_potential, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%soil_moisture_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%soil_moisture_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%soil_moisture_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%soil_moisture_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%soil_moisture_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%soil_moisture_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%soil_moisture_total, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%temperature_veg_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%temperature_veg_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%temperature_veg_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%temperature_veg_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%temperature_veg_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%temperature_veg_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%temperature_veg_2m, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%temperature_bare_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%temperature_bare_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%temperature_bare_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%temperature_bare_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%temperature_bare_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%temperature_bare_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%temperature_bare_2m, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%temperature_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%temperature_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%temperature_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%temperature_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%temperature_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%temperature_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%temperature_2m, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%spec_humidity_veg_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%spec_humidity_veg_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%spec_humidity_veg_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%spec_humidity_veg_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%spec_humidity_veg_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%spec_humidity_veg_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%spec_humidity_veg_2m, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%spec_humidity_bare_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%spec_humidity_bare_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%spec_humidity_bare_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%spec_humidity_bare_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%spec_humidity_bare_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%spec_humidity_bare_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%spec_humidity_bare_2m, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%spec_humidity_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%spec_humidity_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%spec_humidity_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%spec_humidity_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%spec_humidity_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%spec_humidity_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%spec_humidity_2m, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%spec_humidity_surface%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%spec_humidity_surface%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%spec_humidity_surface%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%spec_humidity_surface%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%spec_humidity_surface%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%spec_humidity_surface%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%spec_humidity_surface, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%spec_humid_sfc_veg%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%spec_humid_sfc_veg%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%spec_humid_sfc_veg%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%spec_humid_sfc_veg%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%spec_humid_sfc_veg%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%spec_humid_sfc_veg%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%spec_humid_sfc_veg, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%spec_humid_sfc_bare%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%spec_humid_sfc_bare%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%spec_humid_sfc_bare%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%spec_humid_sfc_bare%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%spec_humid_sfc_bare%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%spec_humid_sfc_bare%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%spec_humid_sfc_bare, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%dewpoint_veg_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%dewpoint_veg_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%dewpoint_veg_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%dewpoint_veg_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%dewpoint_veg_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%dewpoint_veg_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%dewpoint_veg_2m, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%dewpoint_bare_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%dewpoint_bare_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%dewpoint_bare_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%dewpoint_bare_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%dewpoint_bare_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%dewpoint_bare_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%dewpoint_bare_2m, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%diag%dewpoint_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%dewpoint_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%dewpoint_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%dewpoint_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%dewpoint_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%dewpoint_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%diag%dewpoint_2m, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

! Begin noahmp%state variables

  if((noahmp%state%temperature_soil_mp%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_soil_mp%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_soil_mp%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_soil_mp%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_soil_mp%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_soil_mp%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define2dReal(noahmp%state%temperature_soil_mp, ncid, realtype, dim_id_loc, dim_id_soil, dim_id_time, dim_id_hour)

  if((noahmp%state%temperature_snow%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_snow%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_snow%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_snow%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_snow%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_snow%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define2dReal(noahmp%state%temperature_snow, ncid, realtype, dim_id_loc, dim_id_snow, dim_id_time, dim_id_hour)

  if((noahmp%state%temperature_canopy_air%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_canopy_air%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_canopy_air%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_canopy_air%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_canopy_air%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_canopy_air%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%temperature_canopy_air, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%temperature_radiative%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_radiative%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_radiative%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_radiative%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_radiative%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_radiative%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%temperature_radiative, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%temperature_leaf%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_leaf%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_leaf%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_leaf%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_leaf%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_leaf%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%temperature_leaf, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%temperature_ground%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_ground%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_ground%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_ground%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_ground%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_ground%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%temperature_ground, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%temperature_bare_grd%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_bare_grd%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_bare_grd%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_bare_grd%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_bare_grd%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_bare_grd%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%temperature_bare_grd, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%temperature_veg_grd%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_veg_grd%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_veg_grd%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_veg_grd%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_veg_grd%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_veg_grd%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%temperature_veg_grd, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%vapor_pres_canopy_air%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%vapor_pres_canopy_air%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%vapor_pres_canopy_air%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%vapor_pres_canopy_air%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%vapor_pres_canopy_air%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%vapor_pres_canopy_air%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%vapor_pres_canopy_air, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%soil_liquid_vol_mp%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%soil_liquid_vol_mp%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%soil_liquid_vol_mp%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%soil_liquid_vol_mp%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%soil_liquid_vol_mp%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%soil_liquid_vol_mp%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define2dReal(noahmp%state%soil_liquid_vol_mp, ncid, realtype, dim_id_loc, dim_id_soil, dim_id_time, dim_id_hour)

  if((noahmp%state%soil_moisture_vol_mp%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%soil_moisture_vol_mp%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%soil_moisture_vol_mp%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%soil_moisture_vol_mp%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%soil_moisture_vol_mp%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%soil_moisture_vol_mp%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define2dReal(noahmp%state%soil_moisture_vol_mp, ncid, realtype, dim_id_loc, dim_id_soil, dim_id_time, dim_id_hour)

  if((noahmp%state%snow_water_equiv%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%snow_water_equiv%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%snow_water_equiv%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%snow_water_equiv%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%snow_water_equiv%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%snow_water_equiv%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%snow_water_equiv, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%snow_level_ice%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%snow_level_ice%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%snow_level_ice%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%snow_level_ice%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%snow_level_ice%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%snow_level_ice%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define2dReal(noahmp%state%snow_level_ice, ncid, realtype, dim_id_loc, dim_id_snow, dim_id_time, dim_id_hour)

  if((noahmp%state%snow_level_liquid%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%snow_level_liquid%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%snow_level_liquid%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%snow_level_liquid%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%snow_level_liquid%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%snow_level_liquid%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define2dReal(noahmp%state%snow_level_liquid, ncid, realtype, dim_id_loc, dim_id_snow, dim_id_time, dim_id_hour)

  if((noahmp%state%canopy_liquid%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%canopy_liquid%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%canopy_liquid%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%canopy_liquid%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%canopy_liquid%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%canopy_liquid%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%canopy_liquid, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%canopy_ice%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%canopy_ice%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%canopy_ice%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%canopy_ice%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%canopy_ice%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%canopy_ice%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%canopy_ice, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%aquifer_water%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%aquifer_water%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%aquifer_water%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%aquifer_water%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%aquifer_water%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%aquifer_water%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%aquifer_water, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%saturated_water%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%saturated_water%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%saturated_water%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%saturated_water%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%saturated_water%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%saturated_water%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%saturated_water, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%lake_water%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%lake_water%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%lake_water%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%lake_water%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%lake_water%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%lake_water%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%lake_water, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%soil_moisture_wtd%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%soil_moisture_wtd%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%soil_moisture_wtd%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%soil_moisture_wtd%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%soil_moisture_wtd%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%soil_moisture_wtd%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%soil_moisture_wtd, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%eq_soil_water_vol%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%eq_soil_water_vol%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%eq_soil_water_vol%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%eq_soil_water_vol%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%eq_soil_water_vol%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%eq_soil_water_vol%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define2dReal(noahmp%state%eq_soil_water_vol, ncid, realtype, dim_id_loc, dim_id_soil, dim_id_time, dim_id_hour)

  if((noahmp%state%leaf_carbon%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%leaf_carbon%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%leaf_carbon%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%leaf_carbon%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%leaf_carbon%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%leaf_carbon%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%leaf_carbon, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%root_carbon%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%root_carbon%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%root_carbon%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%root_carbon%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%root_carbon%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%root_carbon%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%root_carbon, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%stem_carbon%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%stem_carbon%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%stem_carbon%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%stem_carbon%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%stem_carbon%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%stem_carbon%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%stem_carbon, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%wood_carbon%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%wood_carbon%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%wood_carbon%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%wood_carbon%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%wood_carbon%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%wood_carbon%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%wood_carbon, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%soil_carbon_stable%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%soil_carbon_stable%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%soil_carbon_stable%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%soil_carbon_stable%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%soil_carbon_stable%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%soil_carbon_stable%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%soil_carbon_stable, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%soil_carbon_fast%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%soil_carbon_fast%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%soil_carbon_fast%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%soil_carbon_fast%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%soil_carbon_fast%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%soil_carbon_fast%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%soil_carbon_fast, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%grain_carbon%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%grain_carbon%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%grain_carbon%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%grain_carbon%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%grain_carbon%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%grain_carbon%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%grain_carbon, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%foliage_nitrogen%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%foliage_nitrogen%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%foliage_nitrogen%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%foliage_nitrogen%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%foliage_nitrogen%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%foliage_nitrogen%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%foliage_nitrogen, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%snow_water_equiv_old%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%snow_water_equiv_old%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%snow_water_equiv_old%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%snow_water_equiv_old%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%snow_water_equiv_old%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%snow_water_equiv_old%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%snow_water_equiv_old, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%snow_depth%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%snow_depth%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%snow_depth%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%snow_depth%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%snow_depth%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%snow_depth%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%snow_depth, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%state%snow_age%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%snow_age%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%snow_age%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%snow_age%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%snow_age%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%snow_age%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%state%snow_age, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

! Begin noahmp%flux variables

  if((noahmp%flux%sw_absorbed_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sw_absorbed_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sw_absorbed_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sw_absorbed_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sw_absorbed_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sw_absorbed_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%sw_absorbed_total, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%sw_reflected_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sw_reflected_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sw_reflected_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sw_reflected_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sw_reflected_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sw_reflected_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%sw_reflected_total, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%lw_absorbed_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%lw_absorbed_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%lw_absorbed_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%lw_absorbed_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%lw_absorbed_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%lw_absorbed_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%lw_absorbed_total, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%sensible_heat_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sensible_heat_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sensible_heat_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sensible_heat_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sensible_heat_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sensible_heat_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%sensible_heat_total, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%transpiration_heat%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%transpiration_heat%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%transpiration_heat%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%transpiration_heat%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%transpiration_heat%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%transpiration_heat%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%transpiration_heat, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%latent_heat_canopy%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%latent_heat_canopy%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%latent_heat_canopy%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%latent_heat_canopy%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%latent_heat_canopy%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%latent_heat_canopy%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%latent_heat_canopy, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%latent_heat_ground%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%latent_heat_ground%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%latent_heat_ground%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%latent_heat_ground%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%latent_heat_ground%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%latent_heat_ground%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%latent_heat_ground, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%latent_heat_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%latent_heat_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%latent_heat_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%latent_heat_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%latent_heat_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%latent_heat_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%latent_heat_total, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%ground_heat_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%ground_heat_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%ground_heat_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%ground_heat_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%ground_heat_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%ground_heat_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%ground_heat_total, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%precip_adv_heat_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%precip_adv_heat_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%precip_adv_heat_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%precip_adv_heat_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%precip_adv_heat_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%precip_adv_heat_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%precip_adv_heat_total, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%sw_absorbed_veg%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sw_absorbed_veg%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sw_absorbed_veg%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sw_absorbed_veg%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sw_absorbed_veg%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sw_absorbed_veg%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%sw_absorbed_veg, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%sw_absorbed_ground%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sw_absorbed_ground%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sw_absorbed_ground%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sw_absorbed_ground%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sw_absorbed_ground%diurnal_flag .and. io_type == diurnal ) .or. &
     (noahmp%flux%sw_absorbed_ground%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%sw_absorbed_ground, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%lw_absorbed_grd_veg%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%lw_absorbed_grd_veg%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%lw_absorbed_grd_veg%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%lw_absorbed_grd_veg%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%lw_absorbed_grd_veg%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%lw_absorbed_grd_veg%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%lw_absorbed_grd_veg, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%lw_absorbed_leaf%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%lw_absorbed_leaf%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%lw_absorbed_leaf%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%lw_absorbed_leaf%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%lw_absorbed_leaf%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%lw_absorbed_leaf%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%lw_absorbed_leaf, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%lw_absorbed_grd_bare%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%lw_absorbed_grd_bare%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%lw_absorbed_grd_bare%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%lw_absorbed_grd_bare%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%lw_absorbed_grd_bare%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%lw_absorbed_grd_bare%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%lw_absorbed_grd_bare, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%sensible_heat_grd_veg%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sensible_heat_grd_veg%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sensible_heat_grd_veg%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sensible_heat_grd_veg%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sensible_heat_grd_veg%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sensible_heat_grd_veg%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%sensible_heat_grd_veg, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%sensible_heat_leaf%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sensible_heat_leaf%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sensible_heat_leaf%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sensible_heat_leaf%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sensible_heat_leaf%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sensible_heat_leaf%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%sensible_heat_leaf, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%sensible_heat_grd_bar%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sensible_heat_grd_bar%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sensible_heat_grd_bar%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sensible_heat_grd_bar%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sensible_heat_grd_bar%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sensible_heat_grd_bar%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%sensible_heat_grd_bar, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%latent_heat_trans%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%latent_heat_trans%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%latent_heat_trans%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%latent_heat_trans%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%latent_heat_trans%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%latent_heat_trans%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%latent_heat_trans, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%latent_heat_leaf%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%latent_heat_leaf%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%latent_heat_leaf%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%latent_heat_leaf%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%latent_heat_leaf%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%latent_heat_leaf%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%latent_heat_leaf, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%latent_heat_grd_veg%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%latent_heat_grd_veg%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%latent_heat_grd_veg%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%latent_heat_grd_veg%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%latent_heat_grd_veg%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%latent_heat_grd_veg%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%latent_heat_grd_veg, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%latent_heat_grd_bare%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%latent_heat_grd_bare%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%latent_heat_grd_bare%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%latent_heat_grd_bare%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%latent_heat_grd_bare%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%latent_heat_grd_bare%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%latent_heat_grd_bare, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%snow_sublimation%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%snow_sublimation%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%snow_sublimation%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%snow_sublimation%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%snow_sublimation%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%snow_sublimation%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%snow_sublimation, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%ground_heat_veg%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%ground_heat_veg%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%ground_heat_veg%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%ground_heat_veg%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%ground_heat_veg%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%ground_heat_veg%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%ground_heat_veg, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%ground_heat_bare%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%ground_heat_bare%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%ground_heat_bare%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%ground_heat_bare%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%ground_heat_bare%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%ground_heat_bare%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%ground_heat_bare, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%precip_adv_heat_veg%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%precip_adv_heat_veg%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%precip_adv_heat_veg%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%precip_adv_heat_veg%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%precip_adv_heat_veg%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%precip_adv_heat_veg%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%precip_adv_heat_veg, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%precip_adv_heat_grd_v%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%precip_adv_heat_grd_v%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%precip_adv_heat_grd_v%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%precip_adv_heat_grd_v%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%precip_adv_heat_grd_v%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%precip_adv_heat_grd_v%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%precip_adv_heat_grd_v, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%precip_adv_heat_grd_b%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%precip_adv_heat_grd_b%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%precip_adv_heat_grd_b%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%precip_adv_heat_grd_b%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%precip_adv_heat_grd_b%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%precip_adv_heat_grd_b%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%precip_adv_heat_grd_b, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%transpiration%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%transpiration%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%transpiration%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%transpiration%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%transpiration%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%transpiration%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%transpiration, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%evaporation_canopy%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%evaporation_canopy%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%evaporation_canopy%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%evaporation_canopy%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%evaporation_canopy%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%evaporation_canopy%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%evaporation_canopy, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%evaporation_soil%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%evaporation_soil%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%evaporation_soil%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%evaporation_soil%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%evaporation_soil%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%evaporation_soil%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%evaporation_soil, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%runoff_surface%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%runoff_surface%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%runoff_surface%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%runoff_surface%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%runoff_surface%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%runoff_surface%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%runoff_surface, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%runoff_baseflow%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%runoff_baseflow%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%runoff_baseflow%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%runoff_baseflow%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%runoff_baseflow%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%runoff_baseflow%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%runoff_baseflow, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%snowmelt_out%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%snowmelt_out%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%snowmelt_out%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%snowmelt_out%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%snowmelt_out%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%snowmelt_out%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%snowmelt_out, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%snowmelt_shallow%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%snowmelt_shallow%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%snowmelt_shallow%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%snowmelt_shallow%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%snowmelt_shallow%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%snowmelt_shallow%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%snowmelt_shallow, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%snowmelt_shallow_1%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%snowmelt_shallow_1%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%snowmelt_shallow_1%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%snowmelt_shallow_1%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%snowmelt_shallow_1%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%snowmelt_shallow_1%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%snowmelt_shallow_1, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%snowmelt_shallow_2%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%snowmelt_shallow_2%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%snowmelt_shallow_2%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%snowmelt_shallow_2%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%snowmelt_shallow_2%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%snowmelt_shallow_2%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%snowmelt_shallow_2, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%deep_recharge%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%deep_recharge%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%deep_recharge%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%deep_recharge%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%deep_recharge%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%deep_recharge%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%deep_recharge, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%recharge%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%recharge%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%recharge%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%recharge%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%recharge%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%recharge%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%recharge, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%par_absorbed%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%par_absorbed%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%par_absorbed%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%par_absorbed%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%par_absorbed%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%par_absorbed%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%par_absorbed, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%photosynthesis%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%photosynthesis%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%photosynthesis%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%photosynthesis%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%photosynthesis%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%photosynthesis%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%photosynthesis, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%net_eco_exchange%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%net_eco_exchange%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%net_eco_exchange%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%net_eco_exchange%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%net_eco_exchange%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%net_eco_exchange%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%net_eco_exchange, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%global_prim_prod%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%global_prim_prod%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%global_prim_prod%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%global_prim_prod%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%global_prim_prod%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%global_prim_prod%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%global_prim_prod, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%net_prim_prod%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%net_prim_prod%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%net_prim_prod%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%net_prim_prod%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%net_prim_prod%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%net_prim_prod%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%net_prim_prod, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  if((noahmp%flux%canopy_heat_storage%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%canopy_heat_storage%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%canopy_heat_storage%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%canopy_heat_storage%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%canopy_heat_storage%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%canopy_heat_storage%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Define1dReal(noahmp%flux%canopy_heat_storage, ncid, realtype, dim_id_loc, dim_id_time, dim_id_hour)

  end subroutine DefineNoahMP

  subroutine WriteNoahMP(io_type, namelist, noahmp, ncid, output_counter)
  
  use NamelistRead
  use ufsLandNoahMPType
  use ufsLandNetcdf
  
  type(namelist_type)  :: namelist
  type(noahmp_type)    :: noahmp

  integer :: io_type
  integer :: ncid
  integer :: output_counter
  integer :: local_start

  local_start = namelist%subset_start - namelist%location_start + 1
 
! Begin noahmp%static variables

  if((noahmp%static%vegetation_category%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%vegetation_category%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%vegetation_category%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%vegetation_category%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%vegetation_category%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%vegetation_category%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dInt(io_type, noahmp%static%vegetation_category, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%static%soil_category%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%soil_category%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%soil_category%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%soil_category%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%soil_category%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%soil_category%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dInt(io_type, noahmp%static%soil_category, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%static%slope_category%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%slope_category%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%slope_category%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%slope_category%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%slope_category%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%slope_category%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dInt(io_type, noahmp%static%slope_category, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%static%soil_color_category%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%soil_color_category%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%soil_color_category%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%soil_color_category%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%soil_color_category%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%soil_color_category%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dInt(io_type, noahmp%static%soil_color_category, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%static%soil_interface_depth%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%soil_interface_depth%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%soil_interface_depth%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%soil_interface_depth%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%soil_interface_depth%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%soil_interface_depth%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%static%soil_interface_depth, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/noahmp%static%soil_levels, 1/))

  if((noahmp%static%ice_flag%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%ice_flag%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%ice_flag%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%ice_flag%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%ice_flag%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%ice_flag%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dInt(io_type, noahmp%static%ice_flag, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%static%surface_type%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%surface_type%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%surface_type%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%surface_type%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%surface_type%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%surface_type%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dInt(io_type, noahmp%static%surface_type, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%static%crop_type%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%crop_type%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%crop_type%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%crop_type%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%crop_type%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%crop_type%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dInt(io_type, noahmp%static%crop_type, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%static%temperature_soil_bot%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%static%temperature_soil_bot%output_flag       .and. io_type == output       ) .or. &
     (noahmp%static%temperature_soil_bot%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%static%temperature_soil_bot%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%static%temperature_soil_bot%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%static%temperature_soil_bot%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%static%temperature_soil_bot, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

! Begin noahmp%model variables

  if((noahmp%model%latitude%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%latitude%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%latitude%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%latitude%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%latitude%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%latitude%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%latitude, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%longitude%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%longitude%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%longitude%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%longitude%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%longitude%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%longitude%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%longitude, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%solar_noon_hour%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%solar_noon_hour%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%solar_noon_hour%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%solar_noon_hour%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%solar_noon_hour%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%solar_noon_hour%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dInt(io_type, noahmp%model%solar_noon_hour, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%cosine_zenith%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%cosine_zenith%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%cosine_zenith%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%cosine_zenith%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%cosine_zenith%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%cosine_zenith%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%cosine_zenith, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%forcing_height%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%forcing_height%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%forcing_height%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%forcing_height%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%forcing_height%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%forcing_height%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%forcing_height, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%vegetation_fraction%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%vegetation_fraction%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%vegetation_fraction%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%vegetation_fraction%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%vegetation_fraction%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%vegetation_fraction%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%vegetation_fraction, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%max_vegetation_frac%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%max_vegetation_frac%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%max_vegetation_frac%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%max_vegetation_frac%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%max_vegetation_frac%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%max_vegetation_frac%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%max_vegetation_frac, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%active_snow_levels%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%active_snow_levels%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%active_snow_levels%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%active_snow_levels%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%active_snow_levels%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%active_snow_levels%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%active_snow_levels, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%interface_depth%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%interface_depth%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%interface_depth%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%interface_depth%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%interface_depth%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%interface_depth%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write2dReal(io_type, noahmp%model%interface_depth, ncid, namelist%num_diurnal,   &
      start_in = (/local_start           ,                           1, output_counter/) , &
      count_in = (/namelist%subset_length, noahmp%static%soil_levels+3,              1/))

  if((noahmp%model%snow_soil_thickness%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%snow_soil_thickness%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%snow_soil_thickness%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%snow_soil_thickness%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%snow_soil_thickness%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%snow_soil_thickness%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write2dReal(io_type, noahmp%model%snow_soil_thickness, ncid, namelist%num_diurnal,   &
      start_in = (/local_start           ,                           1, output_counter/) , &
      count_in = (/namelist%subset_length, noahmp%static%soil_levels+3,              1/))

  if((noahmp%model%leaf_area_index%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%leaf_area_index%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%leaf_area_index%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%leaf_area_index%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%leaf_area_index%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%leaf_area_index%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%leaf_area_index, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%stem_area_index%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%stem_area_index%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%stem_area_index%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%stem_area_index%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%stem_area_index%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%stem_area_index%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%stem_area_index, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%growing_deg_days%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%growing_deg_days%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%growing_deg_days%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%growing_deg_days%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%growing_deg_days%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%growing_deg_days%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%growing_deg_days, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%plant_growth_stage%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%plant_growth_stage%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%plant_growth_stage%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%plant_growth_stage%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%plant_growth_stage%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%plant_growth_stage%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dInt(io_type, noahmp%model%plant_growth_stage, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%cm_noahmp%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%cm_noahmp%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%cm_noahmp%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%cm_noahmp%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%cm_noahmp%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%cm_noahmp%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%cm_noahmp, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%ch_noahmp%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%ch_noahmp%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%ch_noahmp%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%ch_noahmp%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%ch_noahmp%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%ch_noahmp%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%ch_noahmp, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%ch_vegetated%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%ch_vegetated%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%ch_vegetated%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%ch_vegetated%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%ch_vegetated%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%ch_vegetated%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%ch_vegetated, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%ch_bare_ground%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%ch_bare_ground%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%ch_bare_ground%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%ch_bare_ground%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%ch_bare_ground%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%ch_bare_ground%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%ch_bare_ground, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%ch_leaf%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%ch_leaf%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%ch_leaf%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%ch_leaf%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%ch_leaf%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%ch_leaf%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%ch_leaf, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%ch_below_canopy%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%ch_below_canopy%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%ch_below_canopy%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%ch_below_canopy%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%ch_below_canopy%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%ch_below_canopy%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%ch_below_canopy, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%ch_vegetated_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%ch_vegetated_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%ch_vegetated_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%ch_vegetated_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%ch_vegetated_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%ch_vegetated_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%ch_vegetated_2m, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%ch_bare_ground_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%ch_bare_ground_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%ch_bare_ground_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%ch_bare_ground_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%ch_bare_ground_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%ch_bare_ground_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%ch_bare_ground_2m, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%friction_velocity%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%friction_velocity%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%friction_velocity%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%friction_velocity%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%friction_velocity%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%friction_velocity%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%friction_velocity, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%rs_sunlit%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%rs_sunlit%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%rs_sunlit%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%rs_sunlit%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%rs_sunlit%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%rs_sunlit%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%rs_sunlit, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%rs_shaded%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%rs_shaded%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%rs_shaded%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%rs_shaded%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%rs_shaded%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%rs_shaded%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%rs_shaded, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%leaf_air_resistance%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%leaf_air_resistance%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%leaf_air_resistance%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%leaf_air_resistance%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%leaf_air_resistance%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%leaf_air_resistance%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%leaf_air_resistance, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%pbl_height%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%pbl_height%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%pbl_height%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%pbl_height%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%pbl_height%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%pbl_height%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%pbl_height, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%mo_length_inverse%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%mo_length_inverse%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%mo_length_inverse%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%mo_length_inverse%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%mo_length_inverse%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%mo_length_inverse%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%mo_length_inverse, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%heat_flux_multiplier%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%heat_flux_multiplier%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%heat_flux_multiplier%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%heat_flux_multiplier%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%heat_flux_multiplier%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%heat_flux_multiplier%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%heat_flux_multiplier, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%model%moisture_flux_multiplier%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%model%moisture_flux_multiplier%output_flag       .and. io_type == output       ) .or. &
     (noahmp%model%moisture_flux_multiplier%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%model%moisture_flux_multiplier%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%model%moisture_flux_multiplier%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%model%moisture_flux_multiplier%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%model%moisture_flux_multiplier, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

! Begin noahmp%forcing variables

  if((noahmp%forcing%temperature_forcing%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%temperature_forcing%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%temperature_forcing%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%temperature_forcing%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%temperature_forcing%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%temperature_forcing%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%forcing%temperature_forcing, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%forcing%specific_humidity_forcing%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%specific_humidity_forcing%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%specific_humidity_forcing%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%specific_humidity_forcing%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%specific_humidity_forcing%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%specific_humidity_forcing%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%forcing%specific_humidity_forcing, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%forcing%surface_pressure_forcing%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%surface_pressure_forcing%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%surface_pressure_forcing%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%surface_pressure_forcing%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%surface_pressure_forcing%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%surface_pressure_forcing%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%forcing%surface_pressure_forcing, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%forcing%wind_speed_forcing%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%wind_speed_forcing%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%wind_speed_forcing%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%wind_speed_forcing%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%wind_speed_forcing%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%wind_speed_forcing%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%forcing%wind_speed_forcing, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%forcing%downward_longwave_forcing%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%downward_longwave_forcing%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%downward_longwave_forcing%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%downward_longwave_forcing%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%downward_longwave_forcing%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%downward_longwave_forcing%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%forcing%downward_longwave_forcing, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%forcing%downward_shortwave_forcing%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%downward_shortwave_forcing%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%downward_shortwave_forcing%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%downward_shortwave_forcing%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%downward_shortwave_forcing%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%downward_shortwave_forcing%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%forcing%downward_shortwave_forcing, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%forcing%precipitation_forcing%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%precipitation_forcing%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%precipitation_forcing%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%precipitation_forcing%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%precipitation_forcing%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%precipitation_forcing%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%forcing%precipitation_forcing, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%forcing%precip_convective%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%precip_convective%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%precip_convective%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%precip_convective%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%precip_convective%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%precip_convective%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%forcing%precip_convective, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%forcing%precip_non_convective%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%precip_non_convective%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%precip_non_convective%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%precip_non_convective%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%precip_non_convective%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%precip_non_convective%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%forcing%precip_non_convective, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%forcing%precip_snow%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%precip_snow%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%precip_snow%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%precip_snow%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%precip_snow%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%precip_snow%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%forcing%precip_snow, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%forcing%precip_graupel%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%precip_graupel%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%precip_graupel%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%precip_graupel%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%precip_graupel%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%precip_graupel%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%forcing%precip_graupel, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%forcing%precip_hail%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%precip_hail%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%precip_hail%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%precip_hail%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%precip_hail%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%precip_hail%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%forcing%precip_hail, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%forcing%snowfall%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%snowfall%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%snowfall%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%snowfall%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%snowfall%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%snowfall%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%forcing%snowfall, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%forcing%rainfall%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%forcing%rainfall%output_flag       .and. io_type == output       ) .or. &
     (noahmp%forcing%rainfall%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%forcing%rainfall%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%forcing%rainfall%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%forcing%rainfall%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%forcing%rainfall, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

! Begin noahmp%diag variables

  if((noahmp%diag%z0_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%z0_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%z0_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%z0_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%z0_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%z0_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%z0_total, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%z0h_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%z0h_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%z0h_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%z0h_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%z0h_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%z0h_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%z0h_total, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%albedo_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%albedo_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%albedo_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%albedo_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%albedo_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%albedo_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%albedo_total, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%albedo_direct%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%albedo_direct%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%albedo_direct%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%albedo_direct%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%albedo_direct%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%albedo_direct%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write2dReal(io_type, noahmp%diag%albedo_direct, ncid, namelist%num_diurnal,   &
      start_in = (/local_start           , 1, output_counter/) , &
      count_in = (/namelist%subset_length, 2,              1/))

  if((noahmp%diag%albedo_diffuse%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%albedo_diffuse%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%albedo_diffuse%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%albedo_diffuse%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%albedo_diffuse%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%albedo_diffuse%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write2dReal(io_type, noahmp%diag%albedo_diffuse, ncid, namelist%num_diurnal,   &
      start_in = (/local_start           , 1, output_counter/) , &
      count_in = (/namelist%subset_length, 2,              1/))

  if((noahmp%diag%albedo_direct_snow%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%albedo_direct_snow%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%albedo_direct_snow%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%albedo_direct_snow%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%albedo_direct_snow%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%albedo_direct_snow%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write2dReal(io_type, noahmp%diag%albedo_direct_snow, ncid, namelist%num_diurnal,   &
      start_in = (/local_start           , 1, output_counter/) , &
      count_in = (/namelist%subset_length, 2,              1/))

  if((noahmp%diag%albedo_diffuse_snow%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%albedo_diffuse_snow%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%albedo_diffuse_snow%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%albedo_diffuse_snow%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%albedo_diffuse_snow%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%albedo_diffuse_snow%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write2dReal(io_type, noahmp%diag%albedo_diffuse_snow, ncid, namelist%num_diurnal,   &
      start_in = (/local_start           , 1, output_counter/) , &
      count_in = (/namelist%subset_length, 2,              1/))

  if((noahmp%diag%emissivity_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%emissivity_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%emissivity_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%emissivity_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%emissivity_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%emissivity_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%emissivity_total, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%canopy_gap_fraction%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%canopy_gap_fraction%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%canopy_gap_fraction%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%canopy_gap_fraction%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%canopy_gap_fraction%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%canopy_gap_fraction%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%canopy_gap_fraction, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%incanopy_gap_fraction%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%incanopy_gap_fraction%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%incanopy_gap_fraction%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%incanopy_gap_fraction%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%incanopy_gap_fraction%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%incanopy_gap_fraction%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%incanopy_gap_fraction, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%precip_frozen_frac%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%precip_frozen_frac%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%precip_frozen_frac%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%precip_frozen_frac%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%precip_frozen_frac%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%precip_frozen_frac%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%precip_frozen_frac, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%snow_cover_fraction%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%snow_cover_fraction%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%snow_cover_fraction%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%snow_cover_fraction%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%snow_cover_fraction%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%snow_cover_fraction%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%snow_cover_fraction, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%canopy_wet_fraction%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%canopy_wet_fraction%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%canopy_wet_fraction%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%canopy_wet_fraction%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%canopy_wet_fraction%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%canopy_wet_fraction%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%canopy_wet_fraction, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%canopy_water%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%canopy_water%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%canopy_water%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%canopy_water%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%canopy_water%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%canopy_water%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%canopy_water, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%depth_water_table%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%depth_water_table%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%depth_water_table%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%depth_water_table%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%depth_water_table%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%depth_water_table%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%depth_water_table, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%lai_sunlit%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%lai_sunlit%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%lai_sunlit%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%lai_sunlit%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%lai_sunlit%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%lai_sunlit%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%lai_sunlit, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%lai_shaded%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%lai_shaded%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%lai_shaded%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%lai_shaded%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%lai_shaded%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%lai_shaded%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%lai_shaded, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%snow_ice_frac_old%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%snow_ice_frac_old%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%snow_ice_frac_old%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%snow_ice_frac_old%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%snow_ice_frac_old%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%snow_ice_frac_old%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write2dReal(io_type, noahmp%diag%snow_ice_frac_old, ncid, namelist%num_diurnal,   &
      start_in = (/local_start           , 1, output_counter/) , &
      count_in = (/namelist%subset_length, 3,             1/))

  if((noahmp%diag%snow_albedo_old%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%snow_albedo_old%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%snow_albedo_old%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%snow_albedo_old%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%snow_albedo_old%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%snow_albedo_old%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%snow_albedo_old, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%evaporation_potential%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%evaporation_potential%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%evaporation_potential%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%evaporation_potential%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%evaporation_potential%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%evaporation_potential%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%evaporation_potential, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%soil_moisture_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%soil_moisture_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%soil_moisture_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%soil_moisture_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%soil_moisture_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%soil_moisture_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%soil_moisture_total, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%temperature_veg_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%temperature_veg_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%temperature_veg_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%temperature_veg_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%temperature_veg_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%temperature_veg_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%temperature_veg_2m, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%temperature_bare_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%temperature_bare_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%temperature_bare_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%temperature_bare_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%temperature_bare_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%temperature_bare_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%temperature_bare_2m, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%temperature_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%temperature_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%temperature_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%temperature_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%temperature_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%temperature_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%temperature_2m, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%spec_humidity_veg_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%spec_humidity_veg_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%spec_humidity_veg_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%spec_humidity_veg_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%spec_humidity_veg_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%spec_humidity_veg_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%spec_humidity_veg_2m, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%spec_humidity_bare_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%spec_humidity_bare_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%spec_humidity_bare_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%spec_humidity_bare_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%spec_humidity_bare_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%spec_humidity_bare_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%spec_humidity_bare_2m, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%spec_humidity_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%spec_humidity_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%spec_humidity_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%spec_humidity_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%spec_humidity_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%spec_humidity_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%spec_humidity_2m, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%spec_humidity_surface%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%spec_humidity_surface%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%spec_humidity_surface%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%spec_humidity_surface%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%spec_humidity_surface%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%spec_humidity_surface%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%spec_humidity_surface, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%spec_humid_sfc_veg%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%spec_humid_sfc_veg%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%spec_humid_sfc_veg%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%spec_humid_sfc_veg%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%spec_humid_sfc_veg%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%spec_humid_sfc_veg%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%spec_humid_sfc_veg, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%spec_humid_sfc_bare%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%spec_humid_sfc_bare%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%spec_humid_sfc_bare%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%spec_humid_sfc_bare%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%spec_humid_sfc_bare%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%spec_humid_sfc_bare%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%spec_humid_sfc_bare, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%dewpoint_veg_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%dewpoint_veg_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%dewpoint_veg_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%dewpoint_veg_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%dewpoint_veg_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%dewpoint_veg_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%dewpoint_veg_2m, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%dewpoint_bare_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%dewpoint_bare_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%dewpoint_bare_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%dewpoint_bare_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%dewpoint_bare_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%dewpoint_bare_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%dewpoint_bare_2m, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%diag%dewpoint_2m%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%diag%dewpoint_2m%output_flag       .and. io_type == output       ) .or. &
     (noahmp%diag%dewpoint_2m%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%diag%dewpoint_2m%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%diag%dewpoint_2m%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%diag%dewpoint_2m%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%diag%dewpoint_2m, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

! Begin noahmp%state variables

  if((noahmp%state%temperature_soil_mp%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_soil_mp%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_soil_mp%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_soil_mp%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_soil_mp%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_soil_mp%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write2dReal(io_type, noahmp%state%temperature_soil_mp, ncid, namelist%num_diurnal,    &
      start_in = (/local_start           ,                         1, output_counter/) , &
      count_in = (/namelist%subset_length, noahmp%static%soil_levels,             1/))

  if((noahmp%state%temperature_snow%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_snow%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_snow%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_snow%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_snow%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_snow%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write2dReal(io_type, noahmp%state%temperature_snow, ncid, namelist%num_diurnal,   &
      start_in = (/local_start           , 1, output_counter/) , &
      count_in = (/namelist%subset_length, 3,             1/))

  if((noahmp%state%temperature_canopy_air%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_canopy_air%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_canopy_air%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_canopy_air%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_canopy_air%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_canopy_air%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%temperature_canopy_air, ncid, namelist%num_diurnal,    &
     start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%temperature_radiative%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_radiative%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_radiative%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_radiative%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_radiative%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_radiative%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%temperature_radiative, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%temperature_leaf%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_leaf%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_leaf%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_leaf%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_leaf%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_leaf%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%temperature_leaf, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%temperature_ground%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_ground%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_ground%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_ground%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_ground%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_ground%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%temperature_ground, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%temperature_bare_grd%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_bare_grd%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_bare_grd%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_bare_grd%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_bare_grd%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_bare_grd%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%temperature_bare_grd, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%temperature_veg_grd%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%temperature_veg_grd%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%temperature_veg_grd%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%temperature_veg_grd%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%temperature_veg_grd%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%temperature_veg_grd%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%temperature_veg_grd, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%vapor_pres_canopy_air%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%vapor_pres_canopy_air%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%vapor_pres_canopy_air%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%vapor_pres_canopy_air%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%vapor_pres_canopy_air%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%vapor_pres_canopy_air%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%vapor_pres_canopy_air, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%soil_liquid_vol_mp%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%soil_liquid_vol_mp%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%soil_liquid_vol_mp%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%soil_liquid_vol_mp%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%soil_liquid_vol_mp%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%soil_liquid_vol_mp%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write2dReal(io_type, noahmp%state%soil_liquid_vol_mp, ncid, namelist%num_diurnal,     &
      start_in = (/local_start           ,                         1, output_counter/) , &
      count_in = (/namelist%subset_length, noahmp%static%soil_levels,             1/))

  if((noahmp%state%soil_moisture_vol_mp%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%soil_moisture_vol_mp%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%soil_moisture_vol_mp%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%soil_moisture_vol_mp%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%soil_moisture_vol_mp%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%soil_moisture_vol_mp%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write2dReal(io_type, noahmp%state%soil_moisture_vol_mp, ncid, namelist%num_diurnal,   &
      start_in = (/local_start           ,                         1, output_counter/) , &
      count_in = (/namelist%subset_length, noahmp%static%soil_levels,             1/))

  if((noahmp%state%snow_water_equiv%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%snow_water_equiv%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%snow_water_equiv%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%snow_water_equiv%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%snow_water_equiv%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%snow_water_equiv%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%snow_water_equiv, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%snow_level_ice%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%snow_level_ice%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%snow_level_ice%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%snow_level_ice%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%snow_level_ice%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%snow_level_ice%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write2dReal(io_type, noahmp%state%snow_level_ice, ncid, namelist%num_diurnal,   &
      start_in = (/local_start           , 1, output_counter/) , &
      count_in = (/namelist%subset_length, 3,              1/))

  if((noahmp%state%snow_level_liquid%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%snow_level_liquid%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%snow_level_liquid%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%snow_level_liquid%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%snow_level_liquid%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%snow_level_liquid%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write2dReal(io_type, noahmp%state%snow_level_liquid, ncid, namelist%num_diurnal,   &
      start_in = (/local_start           , 1, output_counter/) , &
      count_in = (/namelist%subset_length, 3,             1/))

  if((noahmp%state%canopy_liquid%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%canopy_liquid%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%canopy_liquid%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%canopy_liquid%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%canopy_liquid%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%canopy_liquid%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%canopy_liquid, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%canopy_ice%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%canopy_ice%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%canopy_ice%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%canopy_ice%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%canopy_ice%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%canopy_ice%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%canopy_ice, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%aquifer_water%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%aquifer_water%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%aquifer_water%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%aquifer_water%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%aquifer_water%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%aquifer_water%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%aquifer_water, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%saturated_water%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%saturated_water%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%saturated_water%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%saturated_water%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%saturated_water%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%saturated_water%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%saturated_water, ncid, namelist%num_diurnal,    &
     start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%lake_water%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%lake_water%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%lake_water%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%lake_water%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%lake_water%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%lake_water%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%lake_water, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%soil_moisture_wtd%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%soil_moisture_wtd%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%soil_moisture_wtd%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%soil_moisture_wtd%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%soil_moisture_wtd%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%soil_moisture_wtd%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%soil_moisture_wtd, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%eq_soil_water_vol%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%eq_soil_water_vol%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%eq_soil_water_vol%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%eq_soil_water_vol%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%eq_soil_water_vol%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%eq_soil_water_vol%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write2dReal(io_type, noahmp%state%eq_soil_water_vol, ncid, namelist%num_diurnal,   &
      start_in = (/local_start           ,                           1, output_counter/) , &
      count_in = (/namelist%subset_length, noahmp%static%soil_levels  ,              1/))

  if((noahmp%state%leaf_carbon%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%leaf_carbon%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%leaf_carbon%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%leaf_carbon%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%leaf_carbon%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%leaf_carbon%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%leaf_carbon, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%root_carbon%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%root_carbon%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%root_carbon%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%root_carbon%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%root_carbon%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%root_carbon%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%root_carbon, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%stem_carbon%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%stem_carbon%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%stem_carbon%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%stem_carbon%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%stem_carbon%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%stem_carbon%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%stem_carbon, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%wood_carbon%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%wood_carbon%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%wood_carbon%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%wood_carbon%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%wood_carbon%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%wood_carbon%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%wood_carbon, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%soil_carbon_stable%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%soil_carbon_stable%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%soil_carbon_stable%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%soil_carbon_stable%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%soil_carbon_stable%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%soil_carbon_stable%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%soil_carbon_stable, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%soil_carbon_fast%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%soil_carbon_fast%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%soil_carbon_fast%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%soil_carbon_fast%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%soil_carbon_fast%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%soil_carbon_fast%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%soil_carbon_fast, ncid, namelist%num_diurnal,    &
     start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%grain_carbon%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%grain_carbon%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%grain_carbon%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%grain_carbon%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%grain_carbon%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%grain_carbon%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%grain_carbon, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%foliage_nitrogen%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%foliage_nitrogen%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%foliage_nitrogen%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%foliage_nitrogen%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%foliage_nitrogen%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%foliage_nitrogen%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%foliage_nitrogen, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%snow_water_equiv_old%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%snow_water_equiv_old%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%snow_water_equiv_old%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%snow_water_equiv_old%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%snow_water_equiv_old%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%snow_water_equiv_old%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%snow_water_equiv_old, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%snow_depth%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%snow_depth%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%snow_depth%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%snow_depth%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%snow_depth%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%snow_depth%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%snow_depth, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%state%snow_age%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%state%snow_age%output_flag       .and. io_type == output       ) .or. &
     (noahmp%state%snow_age%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%state%snow_age%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%state%snow_age%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%state%snow_age%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%state%snow_age, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

! Begin noahmp%flux variables

  if((noahmp%flux%sw_absorbed_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sw_absorbed_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sw_absorbed_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sw_absorbed_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sw_absorbed_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sw_absorbed_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%sw_absorbed_total, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%sw_reflected_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sw_reflected_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sw_reflected_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sw_reflected_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sw_reflected_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sw_reflected_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%sw_reflected_total, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%lw_absorbed_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%lw_absorbed_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%lw_absorbed_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%lw_absorbed_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%lw_absorbed_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%lw_absorbed_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%lw_absorbed_total, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%sensible_heat_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sensible_heat_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sensible_heat_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sensible_heat_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sensible_heat_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sensible_heat_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%sensible_heat_total, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%transpiration_heat%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%transpiration_heat%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%transpiration_heat%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%transpiration_heat%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%transpiration_heat%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%transpiration_heat%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%transpiration_heat, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%latent_heat_canopy%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%latent_heat_canopy%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%latent_heat_canopy%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%latent_heat_canopy%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%latent_heat_canopy%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%latent_heat_canopy%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%latent_heat_canopy, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%latent_heat_ground%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%latent_heat_ground%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%latent_heat_ground%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%latent_heat_ground%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%latent_heat_ground%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%latent_heat_ground%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%latent_heat_ground, ncid, namelist%num_diurnal,    &
     start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%latent_heat_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%latent_heat_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%latent_heat_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%latent_heat_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%latent_heat_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%latent_heat_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%latent_heat_total, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%ground_heat_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%ground_heat_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%ground_heat_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%ground_heat_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%ground_heat_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%ground_heat_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%ground_heat_total, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%precip_adv_heat_total%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%precip_adv_heat_total%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%precip_adv_heat_total%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%precip_adv_heat_total%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%precip_adv_heat_total%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%precip_adv_heat_total%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%precip_adv_heat_total, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%sw_absorbed_veg%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sw_absorbed_veg%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sw_absorbed_veg%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sw_absorbed_veg%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sw_absorbed_veg%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sw_absorbed_veg%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%sw_absorbed_veg, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

   if((noahmp%flux%sw_absorbed_ground%restart_flag     .and. io_type == restart      ) .or. &
     (noahmp%flux%sw_absorbed_ground%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sw_absorbed_ground%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sw_absorbed_ground%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sw_absorbed_ground%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sw_absorbed_ground%solar_noon_flag   .and. io_type == solar_noon   ) ) &
   call Write1dReal(io_type, noahmp%flux%sw_absorbed_ground, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%lw_absorbed_grd_veg%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%lw_absorbed_grd_veg%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%lw_absorbed_grd_veg%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%lw_absorbed_grd_veg%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%lw_absorbed_grd_veg%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%lw_absorbed_grd_veg%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%lw_absorbed_grd_veg, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

   if((noahmp%flux%lw_absorbed_leaf%restart_flag     .and. io_type == restart      ) .or. &
     (noahmp%flux%lw_absorbed_leaf%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%lw_absorbed_leaf%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%lw_absorbed_leaf%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%lw_absorbed_leaf%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%lw_absorbed_leaf%solar_noon_flag   .and. io_type == solar_noon   ) ) &
   call Write1dReal(io_type, noahmp%flux%lw_absorbed_leaf, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%lw_absorbed_grd_bare%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%lw_absorbed_grd_bare%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%lw_absorbed_grd_bare%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%lw_absorbed_grd_bare%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%lw_absorbed_grd_bare%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%lw_absorbed_grd_bare%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%lw_absorbed_grd_bare, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%sensible_heat_grd_veg%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sensible_heat_grd_veg%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sensible_heat_grd_veg%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sensible_heat_grd_veg%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sensible_heat_grd_veg%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sensible_heat_grd_veg%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%sensible_heat_grd_veg, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%sensible_heat_leaf%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sensible_heat_leaf%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sensible_heat_leaf%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sensible_heat_leaf%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sensible_heat_leaf%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sensible_heat_leaf%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%sensible_heat_leaf, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%sensible_heat_grd_bar%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%sensible_heat_grd_bar%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%sensible_heat_grd_bar%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%sensible_heat_grd_bar%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%sensible_heat_grd_bar%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%sensible_heat_grd_bar%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%sensible_heat_grd_bar, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%latent_heat_trans%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%latent_heat_trans%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%latent_heat_trans%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%latent_heat_trans%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%latent_heat_trans%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%latent_heat_trans%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%latent_heat_trans, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%latent_heat_leaf%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%latent_heat_leaf%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%latent_heat_leaf%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%latent_heat_leaf%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%latent_heat_leaf%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%latent_heat_leaf%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%latent_heat_leaf, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%latent_heat_grd_veg%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%latent_heat_grd_veg%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%latent_heat_grd_veg%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%latent_heat_grd_veg%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%latent_heat_grd_veg%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%latent_heat_grd_veg%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%latent_heat_grd_veg, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%latent_heat_grd_bare%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%latent_heat_grd_bare%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%latent_heat_grd_bare%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%latent_heat_grd_bare%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%latent_heat_grd_bare%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%latent_heat_grd_bare%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%latent_heat_grd_bare, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%snow_sublimation%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%snow_sublimation%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%snow_sublimation%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%snow_sublimation%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%snow_sublimation%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%snow_sublimation%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%snow_sublimation, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%ground_heat_veg%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%ground_heat_veg%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%ground_heat_veg%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%ground_heat_veg%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%ground_heat_veg%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%ground_heat_veg%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%ground_heat_veg, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%ground_heat_bare%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%ground_heat_bare%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%ground_heat_bare%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%ground_heat_bare%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%ground_heat_bare%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%ground_heat_bare%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%ground_heat_bare, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%precip_adv_heat_veg%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%precip_adv_heat_veg%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%precip_adv_heat_veg%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%precip_adv_heat_veg%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%precip_adv_heat_veg%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%precip_adv_heat_veg%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%precip_adv_heat_veg, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%precip_adv_heat_grd_v%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%precip_adv_heat_grd_v%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%precip_adv_heat_grd_v%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%precip_adv_heat_grd_v%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%precip_adv_heat_grd_v%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%precip_adv_heat_grd_v%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%precip_adv_heat_grd_v, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%precip_adv_heat_grd_b%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%precip_adv_heat_grd_b%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%precip_adv_heat_grd_b%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%precip_adv_heat_grd_b%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%precip_adv_heat_grd_b%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%precip_adv_heat_grd_b%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%precip_adv_heat_grd_b, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%transpiration%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%transpiration%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%transpiration%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%transpiration%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%transpiration%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%transpiration%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%transpiration, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%evaporation_canopy%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%evaporation_canopy%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%evaporation_canopy%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%evaporation_canopy%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%evaporation_canopy%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%evaporation_canopy%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%evaporation_canopy, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%evaporation_soil%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%evaporation_soil%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%evaporation_soil%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%evaporation_soil%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%evaporation_soil%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%evaporation_soil%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%evaporation_soil, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%runoff_surface%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%runoff_surface%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%runoff_surface%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%runoff_surface%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%runoff_surface%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%runoff_surface%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%runoff_surface, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%runoff_baseflow%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%runoff_baseflow%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%runoff_baseflow%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%runoff_baseflow%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%runoff_baseflow%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%runoff_baseflow%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%runoff_baseflow, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%snowmelt_out%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%snowmelt_out%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%snowmelt_out%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%snowmelt_out%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%snowmelt_out%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%snowmelt_out%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%snowmelt_out, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%snowmelt_shallow%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%snowmelt_shallow%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%snowmelt_shallow%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%snowmelt_shallow%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%snowmelt_shallow%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%snowmelt_shallow%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%snowmelt_shallow, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%snowmelt_shallow_1%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%snowmelt_shallow_1%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%snowmelt_shallow_1%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%snowmelt_shallow_1%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%snowmelt_shallow_1%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%snowmelt_shallow_1%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%snowmelt_shallow_1, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%snowmelt_shallow_2%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%snowmelt_shallow_2%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%snowmelt_shallow_2%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%snowmelt_shallow_2%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%snowmelt_shallow_2%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%snowmelt_shallow_2%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%snowmelt_shallow_2, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%deep_recharge%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%deep_recharge%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%deep_recharge%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%deep_recharge%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%deep_recharge%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%deep_recharge%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%deep_recharge, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%recharge%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%recharge%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%recharge%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%recharge%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%recharge%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%recharge%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%recharge, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%par_absorbed%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%par_absorbed%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%par_absorbed%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%par_absorbed%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%par_absorbed%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%par_absorbed%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%par_absorbed, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%photosynthesis%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%photosynthesis%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%photosynthesis%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%photosynthesis%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%photosynthesis%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%photosynthesis%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%photosynthesis, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%net_eco_exchange%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%net_eco_exchange%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%net_eco_exchange%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%net_eco_exchange%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%net_eco_exchange%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%net_eco_exchange%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%net_eco_exchange, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%global_prim_prod%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%global_prim_prod%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%global_prim_prod%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%global_prim_prod%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%global_prim_prod%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%global_prim_prod%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%global_prim_prod, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%net_prim_prod%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%net_prim_prod%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%net_prim_prod%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%net_prim_prod%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%net_prim_prod%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%net_prim_prod%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%net_prim_prod, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  if((noahmp%flux%canopy_heat_storage%restart_flag      .and. io_type == restart      ) .or. &
     (noahmp%flux%canopy_heat_storage%output_flag       .and. io_type == output       ) .or. &
     (noahmp%flux%canopy_heat_storage%daily_mean_flag   .and. io_type == daily_mean   ) .or. &
     (noahmp%flux%canopy_heat_storage%monthly_mean_flag .and. io_type == monthly_mean ) .or. &
     (noahmp%flux%canopy_heat_storage%diurnal_flag      .and. io_type == diurnal      ) .or. &
     (noahmp%flux%canopy_heat_storage%solar_noon_flag   .and. io_type == solar_noon   ) ) &
    call Write1dReal(io_type, noahmp%flux%canopy_heat_storage, ncid, namelist%num_diurnal,   &
      start_in = (/local_start,output_counter/), count_in = (/namelist%subset_length, 1/))

  end subroutine WriteNoahMP

  subroutine ReadNoahMP(namelist, noahmp, ncid)
  
  use NamelistRead
  use ufsLandNoahMPType
  use ufsLandNetcdf
  
  type(namelist_type)  :: namelist
  type(noahmp_type)    :: noahmp

  integer :: ncid
  integer :: local_start

  local_start = namelist%subset_start - namelist%location_start + 1

! Begin noahmp%static variables

  if(noahmp%static%vegetation_category%restart_flag) &
    call Read1dInt(noahmp%static%vegetation_category, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%static%soil_category%restart_flag) &
    call Read1dInt(noahmp%static%soil_category, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%static%slope_category%restart_flag) &
    call Read1dInt(noahmp%static%slope_category, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%static%soil_color_category%restart_flag) &
    call Read1dInt(noahmp%static%soil_color_category, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%static%soil_interface_depth%restart_flag) &
    call Read1dReal(noahmp%static%soil_interface_depth, ncid,   &
      start = (/1,1/), count = (/namelist%num_soil_levels_ic, 1/))
!     start = (/1,1/), count = (/noahmp%static%soil_levels, 1/))

  if(noahmp%static%ice_flag%restart_flag) &
    call Read1dInt(noahmp%static%ice_flag, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%static%surface_type%restart_flag) &
    call Read1dInt(noahmp%static%surface_type, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%static%crop_type%restart_flag) &
    call Read1dInt(noahmp%static%crop_type, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%static%temperature_soil_bot%restart_flag) &
    call Read1dReal(noahmp%static%temperature_soil_bot, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%model variables

  if(noahmp%model%latitude%restart_flag) &
    call Read1dReal(noahmp%model%latitude, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%longitude%restart_flag) &
    call Read1dReal(noahmp%model%longitude, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%solar_noon_hour%restart_flag) &
    call Read1dInt(noahmp%model%solar_noon_hour, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%cosine_zenith%restart_flag) &
    call Read1dReal(noahmp%model%cosine_zenith, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%forcing_height%restart_flag) &
    call Read1dReal(noahmp%model%forcing_height, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%vegetation_fraction%restart_flag) &
    call Read1dReal(noahmp%model%vegetation_fraction, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%max_vegetation_frac%restart_flag) &
    call Read1dReal(noahmp%model%max_vegetation_frac, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%active_snow_levels%restart_flag) &
    call Read1dReal(noahmp%model%active_snow_levels, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%interface_depth%restart_flag) &
    call Read2dReal(noahmp%model%interface_depth, ncid,   &
      start = (/local_start           ,                           1, 1/) , &
      count = (/namelist%subset_length, namelist%num_soil_levels_ic+3, 1/))
!     count = (/namelist%subset_length, noahmp%static%soil_levels+3, 1/))

  if(noahmp%model%snow_soil_thickness%restart_flag) &
    call Read2dReal(noahmp%model%snow_soil_thickness, ncid,   &
      start = (/local_start           ,                           1, 1/) , &
      count = (/namelist%subset_length, namelist%num_soil_levels_ic+3, 1/))
!     count = (/namelist%subset_length, noahmp%static%soil_levels+3, 1/))

  if(noahmp%model%leaf_area_index%restart_flag) &
    call Read1dReal(noahmp%model%leaf_area_index, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%stem_area_index%restart_flag) &
    call Read1dReal(noahmp%model%stem_area_index, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%growing_deg_days%restart_flag) &
    call Read1dReal(noahmp%model%growing_deg_days, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%plant_growth_stage%restart_flag) &
    call Read1dInt(noahmp%model%plant_growth_stage, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%cm_noahmp%restart_flag) &
    call Read1dReal(noahmp%model%cm_noahmp, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%ch_noahmp%restart_flag) &
    call Read1dReal(noahmp%model%ch_noahmp, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%ch_vegetated%restart_flag) &
    call Read1dReal(noahmp%model%ch_vegetated, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%ch_bare_ground%restart_flag) &
    call Read1dReal(noahmp%model%ch_bare_ground, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%ch_leaf%restart_flag) &
    call Read1dReal(noahmp%model%ch_leaf, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%ch_below_canopy%restart_flag) &
    call Read1dReal(noahmp%model%ch_below_canopy, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%ch_vegetated_2m%restart_flag) &
    call Read1dReal(noahmp%model%ch_vegetated_2m, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%ch_bare_ground_2m%restart_flag) &
    call Read1dReal(noahmp%model%ch_bare_ground_2m, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%friction_velocity%restart_flag) &
    call Read1dReal(noahmp%model%friction_velocity, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%rs_sunlit%restart_flag) &
    call Read1dReal(noahmp%model%rs_sunlit, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%rs_shaded%restart_flag) &
    call Read1dReal(noahmp%model%rs_shaded, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%leaf_air_resistance%restart_flag) &
    call Read1dReal(noahmp%model%leaf_air_resistance, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%pbl_height%restart_flag) &
    call Read1dReal(noahmp%model%pbl_height, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%mo_length_inverse%restart_flag) &
    call Read1dReal(noahmp%model%mo_length_inverse, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%heat_flux_multiplier%restart_flag) &
    call Read1dReal(noahmp%model%heat_flux_multiplier, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%model%moisture_flux_multiplier%restart_flag) &
    call Read1dReal(noahmp%model%moisture_flux_multiplier, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%forcing variables

  if(noahmp%forcing%temperature_forcing%restart_flag) &
    call Read1dReal(noahmp%forcing%temperature_forcing, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%specific_humidity_forcing%restart_flag) &
    call Read1dReal(noahmp%forcing%specific_humidity_forcing, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%surface_pressure_forcing%restart_flag) &
    call Read1dReal(noahmp%forcing%surface_pressure_forcing, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%wind_speed_forcing%restart_flag) &
    call Read1dReal(noahmp%forcing%wind_speed_forcing, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%downward_longwave_forcing%restart_flag) &
    call Read1dReal(noahmp%forcing%downward_longwave_forcing, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%downward_shortwave_forcing%restart_flag) &
    call Read1dReal(noahmp%forcing%downward_shortwave_forcing, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%precipitation_forcing%restart_flag) &
    call Read1dReal(noahmp%forcing%precipitation_forcing, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%precip_convective%restart_flag) &
    call Read1dReal(noahmp%forcing%precip_convective, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%precip_non_convective%restart_flag) &
    call Read1dReal(noahmp%forcing%precip_non_convective, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%precip_snow%restart_flag) &
    call Read1dReal(noahmp%forcing%precip_snow, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%precip_graupel%restart_flag) &
    call Read1dReal(noahmp%forcing%precip_graupel, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%precip_hail%restart_flag) &
    call Read1dReal(noahmp%forcing%precip_hail, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%snowfall%restart_flag) &
    call Read1dReal(noahmp%forcing%snowfall, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%forcing%rainfall%restart_flag) &
    call Read1dReal(noahmp%forcing%rainfall, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%diag variables

  if(noahmp%diag%z0_total%restart_flag) &
    call Read1dReal(noahmp%diag%z0_total, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%z0h_total%restart_flag) &
    call Read1dReal(noahmp%diag%z0h_total, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%albedo_total%restart_flag) &
    call Read1dReal(noahmp%diag%albedo_total, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%albedo_direct%restart_flag) &
    call Read2dReal(noahmp%diag%albedo_direct, ncid,   &
      start = (/local_start           , 1, 1/) , &
      count = (/namelist%subset_length, 2, 1/))

  if(noahmp%diag%albedo_diffuse%restart_flag) &
    call Read2dReal(noahmp%diag%albedo_diffuse, ncid,   &
      start = (/local_start           , 1, 1/) , &
      count = (/namelist%subset_length, 2, 1/))

  if(noahmp%diag%albedo_direct_snow%restart_flag) &
    call Read2dReal(noahmp%diag%albedo_direct_snow, ncid,   &
      start = (/local_start           , 1, 1/) , &
      count = (/namelist%subset_length, 2, 1/))

  if(noahmp%diag%albedo_diffuse_snow%restart_flag) &
    call Read2dReal(noahmp%diag%albedo_diffuse_snow, ncid,   &
      start = (/local_start           , 1, 1/) , &
      count = (/namelist%subset_length, 2, 1/))

  if(noahmp%diag%emissivity_total%restart_flag) &
    call Read1dReal(noahmp%diag%emissivity_total, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%canopy_gap_fraction%restart_flag) &
    call Read1dReal(noahmp%diag%canopy_gap_fraction, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%incanopy_gap_fraction%restart_flag) &
    call Read1dReal(noahmp%diag%incanopy_gap_fraction, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%precip_frozen_frac%restart_flag) &
    call Read1dReal(noahmp%diag%precip_frozen_frac, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%snow_cover_fraction%restart_flag) &
    call Read1dReal(noahmp%diag%snow_cover_fraction, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%canopy_wet_fraction%restart_flag) &
    call Read1dReal(noahmp%diag%canopy_wet_fraction, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%canopy_water%restart_flag) &
    call Read1dReal(noahmp%diag%canopy_water, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%depth_water_table%restart_flag) &
    call Read1dReal(noahmp%diag%depth_water_table, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%lai_sunlit%restart_flag) &
    call Read1dReal(noahmp%diag%lai_sunlit, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%lai_shaded%restart_flag) &
    call Read1dReal(noahmp%diag%lai_shaded, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%snow_ice_frac_old%restart_flag) &
    call Read2dReal(noahmp%diag%snow_ice_frac_old, ncid,   &
      start = (/local_start           , 1, 1/) , &
      count = (/namelist%subset_length, 3, 1/))

  if(noahmp%diag%snow_albedo_old%restart_flag) &
    call Read1dReal(noahmp%diag%snow_albedo_old, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%evaporation_potential%restart_flag) &
    call Read1dReal(noahmp%diag%evaporation_potential, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%soil_moisture_total%restart_flag) &
    call Read1dReal(noahmp%diag%soil_moisture_total, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%temperature_veg_2m%restart_flag) &
    call Read1dReal(noahmp%diag%temperature_veg_2m, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%temperature_bare_2m%restart_flag) &
    call Read1dReal(noahmp%diag%temperature_bare_2m, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%temperature_2m%restart_flag) &
    call Read1dReal(noahmp%diag%temperature_2m, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%spec_humidity_veg_2m%restart_flag) &
    call Read1dReal(noahmp%diag%spec_humidity_veg_2m, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%spec_humidity_bare_2m%restart_flag) &
    call Read1dReal(noahmp%diag%spec_humidity_bare_2m, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%spec_humidity_2m%restart_flag) &
    call Read1dReal(noahmp%diag%spec_humidity_2m, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%spec_humidity_surface%restart_flag) &
    call Read1dReal(noahmp%diag%spec_humidity_surface, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%spec_humid_sfc_veg%restart_flag) &
    call Read1dReal(noahmp%diag%spec_humid_sfc_veg, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%spec_humid_sfc_bare%restart_flag) &
    call Read1dReal(noahmp%diag%spec_humid_sfc_bare, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%dewpoint_veg_2m%restart_flag) &
    call Read1dReal(noahmp%diag%dewpoint_veg_2m, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%dewpoint_bare_2m%restart_flag) &
    call Read1dReal(noahmp%diag%dewpoint_bare_2m, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%diag%dewpoint_2m%restart_flag) &
    call Read1dReal(noahmp%diag%dewpoint_2m, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%state variables

  if(noahmp%state%temperature_soil_mp%restart_flag) &
!   call Read2dReal(noahmp%state%temperature_soil_mp, ncid,    &
!     start = (/local_start           ,                         1, 1/) , &
!     count = (/namelist%subset_length, noahmp%static%soil_levels, 1/))
    call Read2dReal(noahmp%ic%temperature_soil, ncid,   &
      start = (/local_start           ,                         1, 1/) , &
      count = (/namelist%subset_length, namelist%num_soil_levels_ic, 1/))

  if(noahmp%state%temperature_snow%restart_flag) &
    call Read2dReal(noahmp%state%temperature_snow, ncid,   &
      start = (/local_start           , 1, 1/) , &
      count = (/namelist%subset_length, 3, 1/))

  if(noahmp%state%temperature_canopy_air%restart_flag) &
    call Read1dReal(noahmp%state%temperature_canopy_air, ncid,    &
     start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%temperature_radiative%restart_flag) &
    call Read1dReal(noahmp%state%temperature_radiative, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%temperature_leaf%restart_flag) &
    call Read1dReal(noahmp%state%temperature_leaf, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%temperature_ground%restart_flag) &
    call Read1dReal(noahmp%state%temperature_ground, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%temperature_bare_grd%restart_flag) &
    call Read1dReal(noahmp%state%temperature_bare_grd, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%temperature_veg_grd%restart_flag) &
    call Read1dReal(noahmp%state%temperature_veg_grd, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%vapor_pres_canopy_air%restart_flag) &
    call Read1dReal(noahmp%state%vapor_pres_canopy_air, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%soil_liquid_vol_mp%restart_flag) &
!   call Read2dReal(noahmp%state%soil_liquid_vol_mp, ncid,     &
!     start = (/local_start           ,                         1, 1/) , &
!     count = (/namelist%subset_length, noahmp%static%soil_levels, 1/))
    call Read2dReal(noahmp%ic%soil_liquid_vol, ncid,   &
      start = (/local_start           ,                         1, 1/) , &
      count = (/namelist%subset_length, namelist%num_soil_levels_ic, 1/))

  if(noahmp%state%soil_moisture_vol_mp%restart_flag) &
!   call Read2dReal(noahmp%state%soil_moisture_vol_mp, ncid,   &
!     start = (/local_start           ,                         1, 1/) , &
!     count = (/namelist%subset_length, noahmp%static%soil_levels, 1/))
    call Read2dReal(noahmp%ic%soil_moisture_vol, ncid,   &
      start = (/local_start           ,                         1, 1/) , &
      count = (/namelist%subset_length, namelist%num_soil_levels_ic, 1/))

  if(noahmp%state%snow_water_equiv%restart_flag) &
    call Read1dReal(noahmp%state%snow_water_equiv, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%snow_level_ice%restart_flag) &
    call Read2dReal(noahmp%state%snow_level_ice, ncid,   &
      start = (/local_start           , 1, 1/) , &
      count = (/namelist%subset_length, 3, 1/))

  if(noahmp%state%snow_level_liquid%restart_flag) &
    call Read2dReal(noahmp%state%snow_level_liquid, ncid,   &
      start = (/local_start           , 1, 1/) , &
      count = (/namelist%subset_length, 3, 1/))

  if(noahmp%state%canopy_liquid%restart_flag) &
    call Read1dReal(noahmp%state%canopy_liquid, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%canopy_ice%restart_flag) &
    call Read1dReal(noahmp%state%canopy_ice, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%aquifer_water%restart_flag) &
    call Read1dReal(noahmp%state%aquifer_water, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%saturated_water%restart_flag) &
    call Read1dReal(noahmp%state%saturated_water, ncid,    &
     start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%lake_water%restart_flag) &
    call Read1dReal(noahmp%state%lake_water, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%soil_moisture_wtd%restart_flag) &
    call Read1dReal(noahmp%state%soil_moisture_wtd, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%eq_soil_water_vol%restart_flag) &
    call Read2dReal(noahmp%state%eq_soil_water_vol, ncid,   &
      start = (/local_start           ,                           1, 1/) , &
      count = (/namelist%subset_length, namelist%num_soil_levels_ic  , 1/))
!     count = (/namelist%subset_length, noahmp%static%soil_levels  , 1/))

  if(noahmp%state%leaf_carbon%restart_flag) &
    call Read1dReal(noahmp%state%leaf_carbon, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%root_carbon%restart_flag) &
    call Read1dReal(noahmp%state%root_carbon, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%stem_carbon%restart_flag) &
    call Read1dReal(noahmp%state%stem_carbon, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%wood_carbon%restart_flag) &
    call Read1dReal(noahmp%state%wood_carbon, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%soil_carbon_stable%restart_flag) &
    call Read1dReal(noahmp%state%soil_carbon_stable, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%soil_carbon_fast%restart_flag) &
    call Read1dReal(noahmp%state%soil_carbon_fast, ncid,    &
     start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%grain_carbon%restart_flag) &
    call Read1dReal(noahmp%state%grain_carbon, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%foliage_nitrogen%restart_flag) &
    call Read1dReal(noahmp%state%foliage_nitrogen, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%snow_water_equiv_old%restart_flag) &
    call Read1dReal(noahmp%state%snow_water_equiv_old, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%snow_depth%restart_flag) &
    call Read1dReal(noahmp%state%snow_depth, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%state%snow_age%restart_flag) &
    call Read1dReal(noahmp%state%snow_age, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

! Begin noahmp%flux variables

  if(noahmp%flux%sw_absorbed_total%restart_flag) &
    call Read1dReal(noahmp%flux%sw_absorbed_total, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%sw_reflected_total%restart_flag) &
    call Read1dReal(noahmp%flux%sw_reflected_total, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%lw_absorbed_total%restart_flag) &
    call Read1dReal(noahmp%flux%lw_absorbed_total, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%sensible_heat_total%restart_flag) &
    call Read1dReal(noahmp%flux%sensible_heat_total, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%transpiration_heat%restart_flag) &
    call Read1dReal(noahmp%flux%transpiration_heat, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%latent_heat_canopy%restart_flag) &
    call Read1dReal(noahmp%flux%latent_heat_canopy, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%latent_heat_ground%restart_flag) &
    call Read1dReal(noahmp%flux%latent_heat_ground, ncid,    &
     start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%latent_heat_total%restart_flag) &
    call Read1dReal(noahmp%flux%latent_heat_total, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%ground_heat_total%restart_flag) &
    call Read1dReal(noahmp%flux%ground_heat_total, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%precip_adv_heat_total%restart_flag) &
    call Read1dReal(noahmp%flux%precip_adv_heat_total, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%sw_absorbed_veg%restart_flag) &
    call Read1dReal(noahmp%flux%sw_absorbed_veg, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%sw_absorbed_ground%restart_flag) &
    call Read1dReal(noahmp%flux%sw_absorbed_ground, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%lw_absorbed_grd_veg%restart_flag) &
    call Read1dReal(noahmp%flux%lw_absorbed_grd_veg, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%lw_absorbed_leaf%restart_flag) &
    call Read1dReal(noahmp%flux%lw_absorbed_leaf, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%lw_absorbed_grd_bare%restart_flag) &
    call Read1dReal(noahmp%flux%lw_absorbed_grd_bare, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%sensible_heat_grd_veg%restart_flag) &
    call Read1dReal(noahmp%flux%sensible_heat_grd_veg, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%sensible_heat_leaf%restart_flag) &
    call Read1dReal(noahmp%flux%sensible_heat_leaf, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%sensible_heat_grd_bar%restart_flag) &
    call Read1dReal(noahmp%flux%sensible_heat_grd_bar, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%latent_heat_trans%restart_flag) &
    call Read1dReal(noahmp%flux%latent_heat_trans, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%latent_heat_leaf%restart_flag) &
    call Read1dReal(noahmp%flux%latent_heat_leaf, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%latent_heat_grd_veg%restart_flag) &
    call Read1dReal(noahmp%flux%latent_heat_grd_veg, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%latent_heat_grd_bare%restart_flag) &
    call Read1dReal(noahmp%flux%latent_heat_grd_bare, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%snow_sublimation%restart_flag) &
    call Read1dReal(noahmp%flux%snow_sublimation, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%ground_heat_veg%restart_flag) &
    call Read1dReal(noahmp%flux%ground_heat_veg, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%ground_heat_bare%restart_flag) &
    call Read1dReal(noahmp%flux%ground_heat_bare, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%precip_adv_heat_veg%restart_flag) &
    call Read1dReal(noahmp%flux%precip_adv_heat_veg, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%precip_adv_heat_grd_v%restart_flag) &
    call Read1dReal(noahmp%flux%precip_adv_heat_grd_v, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%precip_adv_heat_grd_b%restart_flag) &
    call Read1dReal(noahmp%flux%precip_adv_heat_grd_b, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%transpiration%restart_flag) &
    call Read1dReal(noahmp%flux%transpiration, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%evaporation_canopy%restart_flag) &
    call Read1dReal(noahmp%flux%evaporation_canopy, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%evaporation_soil%restart_flag) &
    call Read1dReal(noahmp%flux%evaporation_soil, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%runoff_surface%restart_flag) &
    call Read1dReal(noahmp%flux%runoff_surface, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%runoff_baseflow%restart_flag) &
    call Read1dReal(noahmp%flux%runoff_baseflow, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%snowmelt_out%restart_flag) &
    call Read1dReal(noahmp%flux%snowmelt_out, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%snowmelt_shallow%restart_flag) &
    call Read1dReal(noahmp%flux%snowmelt_shallow, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%snowmelt_shallow_1%restart_flag) &
    call Read1dReal(noahmp%flux%snowmelt_shallow_1, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%snowmelt_shallow_2%restart_flag) &
    call Read1dReal(noahmp%flux%snowmelt_shallow_2, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%deep_recharge%restart_flag) &
    call Read1dReal(noahmp%flux%deep_recharge, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%recharge%restart_flag) &
    call Read1dReal(noahmp%flux%recharge, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%par_absorbed%restart_flag) &
    call Read1dReal(noahmp%flux%par_absorbed, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%photosynthesis%restart_flag) &
    call Read1dReal(noahmp%flux%photosynthesis, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%net_eco_exchange%restart_flag) &
    call Read1dReal(noahmp%flux%net_eco_exchange, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%global_prim_prod%restart_flag) &
    call Read1dReal(noahmp%flux%global_prim_prod, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%net_prim_prod%restart_flag) &
    call Read1dReal(noahmp%flux%net_prim_prod, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  if(noahmp%flux%canopy_heat_storage%restart_flag) &
    call Read1dReal(noahmp%flux%canopy_heat_storage, ncid,   &
      start = (/local_start,1/), count = (/namelist%subset_length, 1/))

  end subroutine ReadNoahMP

end module ufsLandGenericIO
