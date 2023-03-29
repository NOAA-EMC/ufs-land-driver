module ufsLandSpecialOutput

  implicit none
  save
  
contains   

  interface accumulate

  subroutine accumulate_int1d(indata, end_of_day, daily_mean_count)
    use ufsLandGenericType, only : int1d
    type(int1d) :: indata
    logical     :: end_of_day
    integer     :: daily_mean_count

    if(indata%daily_mean_flag) then
      if(daily_mean_count == 1) then indata%daily_mean = 0
      indata%daily_mean = indata%daily_mean + indata%data
      if(end_of_day) indata%daily_mean = nint(indata%daily_mean / daily_mean_count)
    end if

  end subroutine accumulate_int1d

  subroutine accumulate_real1d(indata, end_of_day, daily_mean_count)
    use ufsLandGenericType, only : real1d
    type(real1d) :: indata
    logical      :: end_of_day
    integer      :: daily_mean_count

    if(indata%daily_mean_flag) then
      if(daily_mean_count == 1) then indata%daily_mean = 0
      indata%daily_mean = indata%daily_mean + indata%data
      if(end_of_day) indata%daily_mean = indata%daily_mean / daily_mean_count
    end if

  end subroutine accumulate_real1d

  subroutine accumulate_real2d(indata, end_of_day, daily_mean_count)
    use ufsLandGenericType, only : int1d
    type(real2d) :: indata
    logical     :: end_of_day
    integer     :: daily_mean_count

    if(indata%daily_mean_flag) then
      if(daily_mean_count == 1) then indata%daily_mean = 0
      indata%daily_mean = indata%daily_mean + indata%data
      if(end_of_day) indata%daily_mean = indata%daily_mean / daily_mean_count
    end if

  end subroutine accumulate_real2d

  end interface accumulate

  subroutine DailyMeanNoahMP(noahmp, end_of_day, daily_mean_count)
  
  use ufsLandNoahMPType
  
  type(noahmp_type) :: noahmp
  logical           :: end_of_day
  integer           :: daily_mean_count

! Begin noahmp%static variables

  if(noahmp%static%vegetation_category%daily_mean_flag) then
    noahmp%static%vegetation_category%daily_mean = noahmp%static%vegetation_category%daily_mean + &
                                                   noahmp%static%vegetation_category%data
    if(end_of_day) noahmp%static%vegetation_category%daily_mean = &
                     noahmp%static%vegetation_category%daily_mean / daily_mean_count
  end if

  if(noahmp%static%soil_category%daily_mean_flag) then
    noahmp%static%soil_category%daily_mean = noahmp%static%soil_category%daily_mean + &
                                             noahmp%static%soil_category%data
    if(end_of_day) noahmp%static%soil_category%daily_mean = &
                     noahmp%static%soil_category%daily_mean / daily_mean_count
  end if

  if(noahmp%static%slope_category%daily_mean_flag) then
    noahmp%static%slope_category%daily_mean = noahmp%static%slope_category%daily_mean + &
                                              noahmp%static%slope_category%data
    if(end_of_day) noahmp%static%slope_category%daily_mean = &
                     noahmp%static%slope_category%daily_mean / daily_mean_count
  end if

  if(noahmp%static%soil_interface_depth%daily_mean_flag) then
    noahmp%static%soil_interface_depth%daily_mean = noahmp%static%soil_interface_depth%daily_mean + &
                                                    noahmp%static%soil_interface_depth%data
    if(end_of_day) noahmp%static%soil_interface_depth%daily_mean = &
                     noahmp%static%soil_interface_depth%daily_mean / daily_mean_count
  end if

  if(noahmp%static%ice_flag%daily_mean_flag) then
    noahmp%static%ice_flag%daily_mean = noahmp%static%ice_flag%daily_mean + &
                                        noahmp%static%ice_flag%data
    if(end_of_day) noahmp%static%ice_flag%daily_mean = &
                     noahmp%static%ice_flag%daily_mean / daily_mean_count
  end if

  if(noahmp%static%surface_type%daily_mean_flag) then
    noahmp%static%surface_type%daily_mean = noahmp%static%surface_type%daily_mean + &
                                            noahmp%static%surface_type%data
    if(end_of_day) noahmp%static%surface_type%daily_mean = &
                     noahmp%static%surface_type%daily_mean / daily_mean_count
  end if

  if(noahmp%static%crop_type%daily_mean_flag) then
    noahmp%static%crop_type%daily_mean = noahmp%static%crop_type%daily_mean + &
                                         noahmp%static%crop_type%data
    if(end_of_day) noahmp%static%crop_type%daily_mean = &
                     noahmp%static%crop_type%daily_mean / daily_mean_count
  end if

  if(noahmp%static%temperature_soil_bot%daily_mean_flag) then
    noahmp%static%temperature_soil_bot%daily_mean = noahmp%static%temperature_soil_bot%daily_mean + &
                                                    noahmp%static%temperature_soil_bot%data
    if(end_of_day) noahmp%static%temperature_soil_bot%daily_mean = &
                     noahmp%static%temperature_soil_bot%daily_mean / daily_mean_count
  end if

! Begin noahmp%model variables

  if(noahmp%model%latitude%daily_mean_flag) then
    noahmp%model%latitude%daily_mean = noahmp%model%latitude%daily_mean + &
                                       noahmp%model%latitude%data
    if(end_of_day) noahmp%model%latitude%daily_mean = &
                     noahmp%model%latitude%daily_mean / daily_mean_count
  end if

  if(noahmp%model%cosine_zenith%daily_mean_flag) then
    noahmp%model%cosine_zenith%daily_mean = noahmp%model%cosine_zenith%daily_mean + &
                                            noahmp%model%cosine_zenith%data
    if(end_of_day) noahmp%model%cosine_zenith%daily_mean = &
                     noahmp%model%cosine_zenith%daily_mean / daily_mean_count
  end if

  if(noahmp%model%forcing_height%daily_mean_flag) then
    noahmp%model%forcing_height%daily_mean = noahmp%model%forcing_height%daily_mean + &
                                             noahmp%model%forcing_height%data
    if(end_of_day) noahmp%model%forcing_height%daily_mean = &
                     noahmp%model%forcing_height%daily_mean / daily_mean_count
  end if

  if(noahmp%model%vegetation_fraction%daily_mean_flag) then
    noahmp%model%vegetation_fraction%daily_mean = noahmp%model%vegetation_fraction%daily_mean + &
                                                  noahmp%model%vegetation_fraction%data
    if(end_of_day) noahmp%model%vegetation_fraction%daily_mean = &
                     noahmp%model%vegetation_fraction%daily_mean / daily_mean_count
  end if

  if(noahmp%model%max_vegetation_frac%daily_mean_flag) then
    noahmp%model%max_vegetation_frac%daily_mean = noahmp%model%max_vegetation_frac%daily_mean + &
                                                  noahmp%model%max_vegetation_frac%data
    if(end_of_day) noahmp%model%max_vegetation_frac%daily_mean = &
                     noahmp%model%max_vegetation_frac%daily_mean / daily_mean_count
  end if

  if(noahmp%model%active_snow_levels%daily_mean_flag) then
    noahmp%model%active_snow_levels%daily_mean = noahmp%model%active_snow_levels%daily_mean + &
                                                 noahmp%model%active_snow_levels%data
    if(end_of_day) noahmp%model%active_snow_levels%daily_mean = &
                     noahmp%model%active_snow_levels%daily_mean / daily_mean_count
  end if

  if(noahmp%model%interface_depth%daily_mean_flag) then
    noahmp%model%interface_depth%daily_mean = noahmp%model%interface_depth%daily_mean + &
                                              noahmp%model%interface_depth%data
    if(end_of_day) noahmp%model%interface_depth%daily_mean = &
                     noahmp%model%interface_depth%daily_mean / daily_mean_count
  end if

  if(noahmp%model%snow_soil_thickness%daily_mean_flag) then
    noahmp%model%snow_soil_thickness%daily_mean = noahmp%model%snow_soil_thickness%daily_mean + &
                                                  noahmp%model%snow_soil_thickness%data
    if(end_of_day) noahmp%model%snow_soil_thickness%daily_mean = &
                     noahmp%model%snow_soil_thickness%daily_mean / daily_mean_count
  end if

  if(noahmp%model%leaf_area_index%daily_mean_flag) then
    noahmp%model%leaf_area_index%daily_mean = noahmp%model%leaf_area_index%daily_mean + &
                                              noahmp%model%leaf_area_index%data
    if(end_of_day) noahmp%model%leaf_area_index%daily_mean = &
                     noahmp%model%leaf_area_index%daily_mean / daily_mean_count
  end if

  if(noahmp%model%stem_area_index%daily_mean_flag) then
    noahmp%model%stem_area_index%daily_mean = noahmp%model%stem_area_index%daily_mean + &
                                              noahmp%model%stem_area_index%data
    if(end_of_day) noahmp%model%stem_area_index%daily_mean = &
                     noahmp%model%stem_area_index%daily_mean / daily_mean_count
  end if

  if(noahmp%model%growing_deg_days%daily_mean_flag) then
    noahmp%model%growing_deg_days%daily_mean = noahmp%model%growing_deg_days%daily_mean + &
                                               noahmp%model%growing_deg_days%data
    if(end_of_day) noahmp%model%growing_deg_days%daily_mean = &
                     noahmp%model%growing_deg_days%daily_mean / daily_mean_count
  end if

  if(noahmp%model%plant_growth_stage%daily_mean_flag) then
    noahmp%model%plant_growth_stage%daily_mean = noahmp%model%plant_growth_stage%daily_mean + &
                                                 noahmp%model%plant_growth_stage%data
    if(end_of_day) noahmp%model%plant_growth_stage%daily_mean = &
                     noahmp%model%plant_growth_stage%daily_mean / daily_mean_count
  end if

  if(noahmp%model%cm_noahmp%daily_mean_flag) then
    noahmp%model%cm_noahmp%daily_mean = noahmp%model%cm_noahmp%daily_mean + &
                                        noahmp%model%cm_noahmp%data
    if(end_of_day) noahmp%model%cm_noahmp%daily_mean = &
                     noahmp%model%cm_noahmp%daily_mean / daily_mean_count
  end if

  if(noahmp%model%ch_noahmp%daily_mean_flag) then
    noahmp%model%ch_noahmp%daily_mean = noahmp%model%ch_noahmp%daily_mean + &
                                        noahmp%model%ch_noahmp%data
    if(end_of_day) noahmp%model%ch_noahmp%daily_mean = &
                     noahmp%model%ch_noahmp%daily_mean / daily_mean_count
  end if

  if(noahmp%model%ch_vegetated%daily_mean_flag) then
    noahmp%model%ch_vegetated%daily_mean = noahmp%model%ch_vegetated%daily_mean + &
                                           noahmp%model%ch_vegetated%data
    if(end_of_day) noahmp%model%ch_vegetated%daily_mean = &
                     noahmp%model%ch_vegetated%daily_mean / daily_mean_count
  end if

  if(noahmp%model%ch_bare_ground%daily_mean_flag) then
    noahmp%model%ch_bare_ground%daily_mean = noahmp%model%ch_bare_ground%daily_mean + &
                                             noahmp%model%ch_bare_ground%data
    if(end_of_day) noahmp%model%ch_bare_ground%daily_mean = &
                     noahmp%model%ch_bare_ground%daily_mean / daily_mean_count
  end if

  if(noahmp%model%ch_leaf%daily_mean_flag) then
    noahmp%model%ch_leaf%daily_mean = noahmp%model%ch_leaf%daily_mean + &
                                      noahmp%model%ch_leaf%data
    if(end_of_day) noahmp%model%ch_leaf%daily_mean = &
                     noahmp%model%ch_leaf%daily_mean / daily_mean_count
  end if

  if(noahmp%model%ch_below_canopy%daily_mean_flag) then
    noahmp%model%ch_below_canopy%daily_mean = noahmp%model%ch_below_canopy%daily_mean + &
                                              noahmp%model%ch_below_canopy%data
    if(end_of_day) noahmp%model%ch_below_canopy%daily_mean = &
                     noahmp%model%ch_below_canopy%daily_mean / daily_mean_count
  end if

  if(noahmp%model%ch_vegetated_2m%daily_mean_flag) then
    noahmp%model%ch_vegetated_2m%daily_mean = noahmp%model%ch_vegetated_2m%daily_mean + &
                                              noahmp%model%ch_vegetated_2m%data
    if(end_of_day) noahmp%model%ch_vegetated_2m%daily_mean = &
                     noahmp%model%ch_vegetated_2m%daily_mean / daily_mean_count
  end if

  if(noahmp%model%ch_bare_ground_2m%daily_mean_flag) then
    noahmp%model%ch_bare_ground_2m%daily_mean = noahmp%model%ch_bare_ground_2m%daily_mean + &
                                                noahmp%model%ch_bare_ground_2m%data
    if(end_of_day) noahmp%model%ch_bare_ground_2m%daily_mean = &
                     noahmp%model%ch_bare_ground_2m%daily_mean / daily_mean_count
  end if

  if(noahmp%model%friction_velocity%daily_mean_flag) then
    noahmp%model%friction_velocity%daily_mean = noahmp%model%friction_velocity%daily_mean + &
                                                noahmp%model%friction_velocity%data
    if(end_of_day) noahmp%model%friction_velocity%daily_mean = &
                     noahmp%model%friction_velocity%daily_mean / daily_mean_count
  end if

  if(noahmp%model%rs_sunlit%daily_mean_flag) then
    noahmp%model%rs_sunlit%daily_mean = noahmp%model%rs_sunlit%daily_mean + &
                                        noahmp%model%rs_sunlit%data
    if(end_of_day) noahmp%model%rs_sunlit%daily_mean = &
                     noahmp%model%rs_sunlit%daily_mean / daily_mean_count
  end if

  if(noahmp%model%rs_shaded%daily_mean_flag) then
    noahmp%model%rs_shaded%daily_mean = noahmp%model%rs_shaded%daily_mean + &
                                        noahmp%model%rs_shaded%data
    if(end_of_day) noahmp%model%rs_shaded%daily_mean = &
                     noahmp%model%rs_shaded%daily_mean / daily_mean_count
  end if

  if(noahmp%model%leaf_air_resistance%daily_mean_flag) then
    noahmp%model%leaf_air_resistance%daily_mean = noahmp%model%leaf_air_resistance%daily_mean + &
                                                  noahmp%model%leaf_air_resistance%data
    if(end_of_day) noahmp%model%leaf_air_resistance%daily_mean = &
                     noahmp%model%leaf_air_resistance%daily_mean / daily_mean_count
  end if

  if(noahmp%model%pbl_height%daily_mean_flag) then
    noahmp%model%pbl_height%daily_mean = noahmp%model%pbl_height%daily_mean + &
                                         noahmp%model%pbl_height%data
    if(end_of_day) noahmp%model%pbl_height%daily_mean = &
                     noahmp%model%pbl_height%daily_mean / daily_mean_count
  end if

  if(noahmp%model%mo_length_inverse%daily_mean_flag) then
    noahmp%model%mo_length_inverse%daily_mean = noahmp%model%mo_length_inverse%daily_mean + &
                                                noahmp%model%mo_length_inverse%data
    if(end_of_day) noahmp%model%mo_length_inverse%daily_mean = &
                     noahmp%model%mo_length_inverse%daily_mean / daily_mean_count
  end if

  if(noahmp%model%heat_flux_multiplier%daily_mean_flag) then
    noahmp%model%heat_flux_multiplier%daily_mean = noahmp%model%heat_flux_multiplier%daily_mean + &
                                                   noahmp%model%heat_flux_multiplier%data
    if(end_of_day) noahmp%model%heat_flux_multiplier%daily_mean = &
                     noahmp%model%heat_flux_multiplier%daily_mean / daily_mean_count
  end if

  if(noahmp%model%moisture_flux_multiplier%daily_mean_flag) then
    noahmp%model%moisture_flux_multiplier%daily_mean = noahmp%model%moisture_flux_multiplier%daily_mean + &
                                                       noahmp%model%moisture_flux_multiplier%data
    if(end_of_day) noahmp%model%moisture_flux_multiplier%daily_mean = &
                     noahmp%model%moisture_flux_multiplier%daily_mean / daily_mean_count
  end if

! Begin noahmp%forcing variables

  if(noahmp%forcing%temperature_forcing%daily_mean_flag) then
    noahmp%forcing%temperature_forcing%daily_mean = noahmp%forcing%temperature_forcing%daily_mean + &
                                                    noahmp%forcing%temperature_forcing%data
    if(end_of_day) noahmp%forcing%temperature_forcing%daily_mean = &
                     noahmp%forcing%temperature_forcing%daily_mean / daily_mean_count
  end if

  if(noahmp%forcing%specific_humidity_forcing%daily_mean_flag) then
    noahmp%forcing%specific_humidity_forcing%daily_mean = noahmp%forcing%specific_humidity_forcing%daily_mean + &
                                                          noahmp%forcing%specific_humidity_forcing%data
    if(end_of_day) noahmp%forcing%specific_humidity_forcing%daily_mean = &
                     noahmp%forcing%specific_humidity_forcing%daily_mean / daily_mean_count
  end if

  if(noahmp%forcing%surface_pressure_forcing%daily_mean_flag) then
    noahmp%forcing%surface_pressure_forcing%daily_mean = noahmp%forcing%surface_pressure_forcing%daily_mean + &
                                                         noahmp%forcing%surface_pressure_forcing%data
    if(end_of_day) noahmp%forcing%surface_pressure_forcing%daily_mean = &
                     noahmp%forcing%surface_pressure_forcing%daily_mean / daily_mean_count
  end if

  if(noahmp%forcing%wind_speed_forcing%daily_mean_flag) then
    noahmp%forcing%wind_speed_forcing%daily_mean = noahmp%forcing%wind_speed_forcing%daily_mean + &
                                                   noahmp%forcing%wind_speed_forcing%data
    if(end_of_day) noahmp%forcing%wind_speed_forcing%daily_mean = &
                     noahmp%forcing%wind_speed_forcing%daily_mean / daily_mean_count
  end if

  if(noahmp%forcing%downward_longwave_forcing%daily_mean_flag) then
    noahmp%forcing%downward_longwave_forcing%daily_mean = noahmp%forcing%downward_longwave_forcing%daily_mean + &
                                                          noahmp%forcing%downward_longwave_forcing%data
    if(end_of_day) noahmp%forcing%downward_longwave_forcing%daily_mean = &
                     noahmp%forcing%downward_longwave_forcing%daily_mean / daily_mean_count
  end if

  if(noahmp%forcing%downward_shortwave_forcing%daily_mean_flag) then
    noahmp%forcing%downward_shortwave_forcing%daily_mean = noahmp%forcing%downward_shortwave_forcing%daily_mean + &
                                                           noahmp%forcing%downward_shortwave_forcing%data
    if(end_of_day) noahmp%forcing%downward_shortwave_forcing%daily_mean = &
                     noahmp%forcing%downward_shortwave_forcing%daily_mean / daily_mean_count
  end if

  if(noahmp%forcing%precipitation_forcing%daily_mean_flag) then
    noahmp%forcing%precipitation_forcing%daily_mean = noahmp%forcing%precipitation_forcing%daily_mean + &
                                                      noahmp%forcing%precipitation_forcing%data
    if(end_of_day) noahmp%forcing%precipitation_forcing%daily_mean = &
                     noahmp%forcing%precipitation_forcing%daily_mean / daily_mean_count
  end if

  if(noahmp%forcing%precip_convective%daily_mean_flag) then
    noahmp%forcing%precip_convective%daily_mean = noahmp%forcing%precip_convective%daily_mean + &
                                                  noahmp%forcing%precip_convective%data
    if(end_of_day) noahmp%forcing%precip_convective%daily_mean = &
                     noahmp%forcing%precip_convective%daily_mean / daily_mean_count
  end if

  if(noahmp%forcing%precip_non_convective%daily_mean_flag) then
    noahmp%forcing%precip_non_convective%daily_mean = noahmp%forcing%precip_non_convective%daily_mean + &
                                                      noahmp%forcing%precip_non_convective%data
    if(end_of_day) noahmp%forcing%precip_non_convective%daily_mean = &
                     noahmp%forcing%precip_non_convective%daily_mean / daily_mean_count
  end if

  if(noahmp%forcing%precip_snow%daily_mean_flag) then
    noahmp%forcing%precip_snow%daily_mean = noahmp%forcing%precip_snow%daily_mean + &
                                            noahmp%forcing%precip_snow%data
    if(end_of_day) noahmp%forcing%precip_snow%daily_mean = &
                     noahmp%forcing%precip_snow%daily_mean / daily_mean_count
  end if

  if(noahmp%forcing%precip_graupel%daily_mean_flag) then
    noahmp%forcing%precip_graupel%daily_mean = noahmp%forcing%precip_graupel%daily_mean + &
                                               noahmp%forcing%precip_graupel%data
    if(end_of_day) noahmp%forcing%precip_graupel%daily_mean = &
                     noahmp%forcing%precip_graupel%daily_mean / daily_mean_count
  end if

  if(noahmp%forcing%precip_hail%daily_mean_flag) then
    noahmp%forcing%precip_hail%daily_mean = noahmp%forcing%precip_hail%daily_mean + &
                                            noahmp%forcing%precip_hail%data
    if(end_of_day) noahmp%forcing%precip_hail%daily_mean = &
                     noahmp%forcing%precip_hail%daily_mean / daily_mean_count
  end if

  if(noahmp%forcing%snowfall%daily_mean_flag) then
    noahmp%forcing%snowfall%daily_mean = noahmp%forcing%snowfall%daily_mean + &
                                         noahmp%forcing%snowfall%data
    if(end_of_day) noahmp%forcing%snowfall%daily_mean = &
                     noahmp%forcing%snowfall%daily_mean / daily_mean_count
  end if

  if(noahmp%forcing%rainfall%daily_mean_flag) then
    noahmp%forcing%rainfall%daily_mean = noahmp%forcing%rainfall%daily_mean + &
                                         noahmp%forcing%rainfall%data
    if(end_of_day) noahmp%forcing%rainfall%daily_mean = &
                     noahmp%forcing%rainfall%daily_mean / daily_mean_count
  end if

! Begin noahmp%diag variables

  if(noahmp%diag%z0_total%daily_mean_flag) then
    noahmp%diag%z0_total%daily_mean = noahmp%diag%z0_total%daily_mean + &
                                      noahmp%diag%z0_total%data
    if(end_of_day) noahmp%diag%z0_total%daily_mean = &
                     noahmp%diag%z0_total%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%z0h_total%daily_mean_flag) then
    noahmp%diag%z0h_total%daily_mean = noahmp%diag%z0h_total%daily_mean + &
                                       noahmp%diag%z0h_total%data
    if(end_of_day) noahmp%diag%z0h_total%daily_mean = &
                     noahmp%diag%z0h_total%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%albedo_total%daily_mean_flag) then
    noahmp%diag%albedo_total%daily_mean = noahmp%diag%albedo_total%daily_mean + &
                                          noahmp%diag%albedo_total%data
    if(end_of_day) noahmp%diag%albedo_total%daily_mean = &
                     noahmp%diag%albedo_total%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%albedo_direct%daily_mean_flag) then
    noahmp%diag%albedo_direct%daily_mean = noahmp%diag%albedo_direct%daily_mean + &
                                           noahmp%diag%albedo_direct%data
    if(end_of_day) noahmp%diag%albedo_direct%daily_mean = &
                     noahmp%diag%albedo_direct%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%albedo_diffuse%daily_mean_flag) then
    noahmp%diag%albedo_diffuse%daily_mean = noahmp%diag%albedo_diffuse%daily_mean + &
                                            noahmp%diag%albedo_diffuse%data
    if(end_of_day) noahmp%diag%albedo_diffuse%daily_mean = &
                     noahmp%diag%albedo_diffuse%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%albedo_direct_snow%daily_mean_flag) then
    noahmp%diag%albedo_direct_snow%daily_mean = noahmp%diag%albedo_direct_snow%daily_mean + &
                                                noahmp%diag%albedo_direct_snow%data
    if(end_of_day) noahmp%diag%albedo_direct_snow%daily_mean = &
                     noahmp%diag%albedo_direct_snow%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%albedo_diffuse_snow%daily_mean_flag) then
    noahmp%diag%albedo_diffuse_snow%daily_mean = noahmp%diag%albedo_diffuse_snow%daily_mean + &
                                                 noahmp%diag%albedo_diffuse_snow%data
    if(end_of_day) noahmp%diag%albedo_diffuse_snow%daily_mean = &
                     noahmp%diag%albedo_diffuse_snow%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%emissivity_total%daily_mean_flag) then
    noahmp%diag%emissivity_total%daily_mean = noahmp%diag%emissivity_total%daily_mean + &
                                              noahmp%diag%emissivity_total%data
    if(end_of_day) noahmp%diag%emissivity_total%daily_mean = &
                     noahmp%diag%emissivity_total%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%canopy_gap_fraction%daily_mean_flag) then
    noahmp%diag%canopy_gap_fraction%daily_mean = noahmp%diag%canopy_gap_fraction%daily_mean + &
                                                 noahmp%diag%canopy_gap_fraction%data
    if(end_of_day) noahmp%diag%canopy_gap_fraction%daily_mean = &
                     noahmp%diag%canopy_gap_fraction%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%incanopy_gap_fraction%daily_mean_flag) then
    noahmp%diag%incanopy_gap_fraction%daily_mean = noahmp%diag%incanopy_gap_fraction%daily_mean + &
                                                   noahmp%diag%incanopy_gap_fraction%data
    if(end_of_day) noahmp%diag%incanopy_gap_fraction%daily_mean = &
                     noahmp%diag%incanopy_gap_fraction%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%precip_frozen_frac%daily_mean_flag) then
    noahmp%diag%precip_frozen_frac%daily_mean = noahmp%diag%precip_frozen_frac%daily_mean + &
                                                noahmp%diag%precip_frozen_frac%data
    if(end_of_day) noahmp%diag%precip_frozen_frac%daily_mean = &
                     noahmp%diag%precip_frozen_frac%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%snow_cover_fraction%daily_mean_flag) then
    noahmp%diag%snow_cover_fraction%daily_mean = noahmp%diag%snow_cover_fraction%daily_mean + &
                                                 noahmp%diag%snow_cover_fraction%data
    if(end_of_day) noahmp%diag%snow_cover_fraction%daily_mean = &
                     noahmp%diag%snow_cover_fraction%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%canopy_wet_fraction%daily_mean_flag) then
    noahmp%diag%canopy_wet_fraction%daily_mean = noahmp%diag%canopy_wet_fraction%daily_mean + &
                                                 noahmp%diag%canopy_wet_fraction%data
    if(end_of_day) noahmp%diag%canopy_wet_fraction%daily_mean = &
                     noahmp%diag%canopy_wet_fraction%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%canopy_water%daily_mean_flag) then
    noahmp%diag%canopy_water%daily_mean = noahmp%diag%canopy_water%daily_mean + &
                                          noahmp%diag%canopy_water%data
    if(end_of_day) noahmp%diag%canopy_water%daily_mean = &
                     noahmp%diag%canopy_water%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%depth_water_table%daily_mean_flag) then
    noahmp%diag%depth_water_table%daily_mean = noahmp%diag%depth_water_table%daily_mean + &
                                               noahmp%diag%depth_water_table%data
    if(end_of_day) noahmp%diag%depth_water_table%daily_mean = &
                     noahmp%diag%depth_water_table%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%lai_sunlit%daily_mean_flag) then
    noahmp%diag%lai_sunlit%daily_mean = noahmp%diag%lai_sunlit%daily_mean + &
                                        noahmp%diag%lai_sunlit%data
    if(end_of_day) noahmp%diag%lai_sunlit%daily_mean = &
                     noahmp%diag%lai_sunlit%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%lai_shaded%daily_mean_flag) then
    noahmp%diag%lai_shaded%daily_mean = noahmp%diag%lai_shaded%daily_mean + &
                                        noahmp%diag%lai_shaded%data
    if(end_of_day) noahmp%diag%lai_shaded%daily_mean = &
                     noahmp%diag%lai_shaded%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%snow_ice_frac_old%daily_mean_flag) then
    noahmp%diag%snow_ice_frac_old%daily_mean = noahmp%diag%snow_ice_frac_old%daily_mean + &
                                               noahmp%diag%snow_ice_frac_old%data
    if(end_of_day) noahmp%diag%snow_ice_frac_old%daily_mean = &
                     noahmp%diag%snow_ice_frac_old%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%snow_albedo_old%daily_mean_flag) then
    noahmp%diag%snow_albedo_old%daily_mean = noahmp%diag%snow_albedo_old%daily_mean + &
                                             noahmp%diag%snow_albedo_old%data
    if(end_of_day) noahmp%diag%snow_albedo_old%daily_mean = &
                     noahmp%diag%snow_albedo_old%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%evaporation_potential%daily_mean_flag) then
    noahmp%diag%evaporation_potential%daily_mean = noahmp%diag%evaporation_potential%daily_mean + &
                                                   noahmp%diag%evaporation_potential%data
    if(end_of_day) noahmp%diag%evaporation_potential%daily_mean = &
                     noahmp%diag%evaporation_potential%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%soil_moisture_total%daily_mean_flag) then
    noahmp%diag%soil_moisture_total%daily_mean = noahmp%diag%soil_moisture_total%daily_mean + &
                                                 noahmp%diag%soil_moisture_total%data
    if(end_of_day) noahmp%diag%soil_moisture_total%daily_mean = &
                     noahmp%diag%soil_moisture_total%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%temperature_veg_2m%daily_mean_flag) then
    noahmp%diag%temperature_veg_2m%daily_mean = noahmp%diag%temperature_veg_2m%daily_mean + &
                                                noahmp%diag%temperature_veg_2m%data
    if(end_of_day) noahmp%diag%temperature_veg_2m%daily_mean = &
                     noahmp%diag%temperature_veg_2m%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%temperature_bare_2m%daily_mean_flag) then
    noahmp%diag%temperature_bare_2m%daily_mean = noahmp%diag%temperature_bare_2m%daily_mean + &
                                                 noahmp%diag%temperature_bare_2m%data
    if(end_of_day) noahmp%diag%temperature_bare_2m%daily_mean = &
                     noahmp%diag%temperature_bare_2m%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%temperature_2m%daily_mean_flag) then
    noahmp%diag%temperature_2m%daily_mean = noahmp%diag%temperature_2m%daily_mean + &
                                            noahmp%diag%temperature_2m%data
    if(end_of_day) noahmp%diag%temperature_2m%daily_mean = &
                     noahmp%diag%temperature_2m%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%spec_humidity_veg_2m%daily_mean_flag) then
    noahmp%diag%spec_humidity_veg_2m%daily_mean = noahmp%diag%spec_humidity_veg_2m%daily_mean + &
                                                  noahmp%diag%spec_humidity_veg_2m%data
    if(end_of_day) noahmp%diag%spec_humidity_veg_2m%daily_mean = &
                     noahmp%diag%spec_humidity_veg_2m%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%spec_humidity_bare_2m%daily_mean_flag) then
    noahmp%diag%spec_humidity_bare_2m%daily_mean = noahmp%diag%spec_humidity_bare_2m%daily_mean + &
                                                   noahmp%diag%spec_humidity_bare_2m%data
    if(end_of_day) noahmp%diag%spec_humidity_bare_2m%daily_mean = &
                     noahmp%diag%spec_humidity_bare_2m%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%spec_humidity_2m%daily_mean_flag) then
    noahmp%diag%spec_humidity_2m%daily_mean = noahmp%diag%spec_humidity_2m%daily_mean + &
                                              noahmp%diag%spec_humidity_2m%data
    if(end_of_day) noahmp%diag%spec_humidity_2m%daily_mean = &
                     noahmp%diag%spec_humidity_2m%daily_mean / daily_mean_count
  end if

  if(noahmp%diag%spec_humidity_surface%daily_mean_flag) then
    noahmp%diag%spec_humidity_surface%daily_mean = noahmp%diag%spec_humidity_surface%daily_mean + &
                                                   noahmp%diag%spec_humidity_surface%data
    if(end_of_day) noahmp%diag%spec_humidity_surface%daily_mean = &
                     noahmp%diag%spec_humidity_surface%daily_mean / daily_mean_count
  end if

! Begin noahmp%state variables

  if(noahmp%state%temperature_soil%daily_mean_flag) then
    noahmp%state%temperature_soil%daily_mean = noahmp%state%temperature_soil%daily_mean + &
                                               noahmp%state%temperature_soil%data
    if(end_of_day) noahmp%state%temperature_soil%daily_mean = &
                     noahmp%state%temperature_soil%daily_mean / daily_mean_count
  end if

  if(noahmp%state%temperature_snow%daily_mean_flag) then
    noahmp%state%temperature_snow%daily_mean = noahmp%state%temperature_snow%daily_mean + &
                                               noahmp%state%temperature_snow%data
    if(end_of_day) noahmp%state%temperature_snow%daily_mean = &
                     noahmp%state%temperature_snow%daily_mean / daily_mean_count
  end if

  if(noahmp%state%temperature_canopy_air%daily_mean_flag) then
    noahmp%state%temperature_canopy_air%daily_mean = noahmp%state%temperature_canopy_air%daily_mean + &
                                                     noahmp%state%temperature_canopy_air%data
    if(end_of_day) noahmp%state%temperature_canopy_air%daily_mean = &
                     noahmp%state%temperature_canopy_air%daily_mean / daily_mean_count
  end if

  if(noahmp%state%temperature_radiative%daily_mean_flag) then
    noahmp%state%temperature_radiative%daily_mean = noahmp%state%temperature_radiative%daily_mean + &
                                                    noahmp%state%temperature_radiative%data
    if(end_of_day) noahmp%state%temperature_radiative%daily_mean = &
                     noahmp%state%temperature_radiative%daily_mean / daily_mean_count
  end if

  if(noahmp%state%temperature_leaf%daily_mean_flag) then
    noahmp%state%temperature_leaf%daily_mean = noahmp%state%temperature_leaf%daily_mean + &
                                               noahmp%state%temperature_leaf%data
    if(end_of_day) noahmp%state%temperature_leaf%daily_mean = &
                     noahmp%state%temperature_leaf%daily_mean / daily_mean_count
  end if

  if(noahmp%state%temperature_ground%daily_mean_flag) then
    noahmp%state%temperature_ground%daily_mean = noahmp%state%temperature_ground%daily_mean + &
                                                 noahmp%state%temperature_ground%data
    if(end_of_day) noahmp%state%temperature_ground%daily_mean = &
                     noahmp%state%temperature_ground%daily_mean / daily_mean_count
  end if

  if(noahmp%state%temperature_bare_grd%daily_mean_flag) then
    noahmp%state%temperature_bare_grd%daily_mean = noahmp%state%temperature_bare_grd%daily_mean + &
                                                   noahmp%state%temperature_bare_grd%data
    if(end_of_day) noahmp%state%temperature_bare_grd%daily_mean = &
                     noahmp%state%temperature_bare_grd%daily_mean / daily_mean_count
  end if

  if(noahmp%state%temperature_veg_grd%daily_mean_flag) then
    noahmp%state%temperature_veg_grd%daily_mean = noahmp%state%temperature_veg_grd%daily_mean + &
                                                  noahmp%state%temperature_veg_grd%data
    if(end_of_day) noahmp%state%temperature_veg_grd%daily_mean = &
                     noahmp%state%temperature_veg_grd%daily_mean / daily_mean_count
  end if

  if(noahmp%state%vapor_pres_canopy_air%daily_mean_flag) then
    noahmp%state%vapor_pres_canopy_air%daily_mean = noahmp%state%vapor_pres_canopy_air%daily_mean + &
                                                    noahmp%state%vapor_pres_canopy_air%data
    if(end_of_day) noahmp%state%vapor_pres_canopy_air%daily_mean = &
                     noahmp%state%vapor_pres_canopy_air%daily_mean / daily_mean_count
  end if

  if(noahmp%state%soil_liquid_vol%daily_mean_flag) then
    noahmp%state%soil_liquid_vol%daily_mean = noahmp%state%soil_liquid_vol%daily_mean + &
                                              noahmp%state%soil_liquid_vol%data
    if(end_of_day) noahmp%state%soil_liquid_vol%daily_mean = &
                     noahmp%state%soil_liquid_vol%daily_mean / daily_mean_count
  end if

  if(noahmp%state%soil_moisture_vol%daily_mean_flag) then
    noahmp%state%soil_moisture_vol%daily_mean = noahmp%state%soil_moisture_vol%daily_mean + &
                                                noahmp%state%soil_moisture_vol%data
    if(end_of_day) noahmp%state%soil_moisture_vol%daily_mean = &
                     noahmp%state%soil_moisture_vol%daily_mean / daily_mean_count
  end if

  if(noahmp%state%snow_water_equiv%daily_mean_flag) then
    noahmp%state%snow_water_equiv%daily_mean = noahmp%state%snow_water_equiv%daily_mean + &
                                               noahmp%state%snow_water_equiv%data
    if(end_of_day) noahmp%state%snow_water_equiv%daily_mean = &
                     noahmp%state%snow_water_equiv%daily_mean / daily_mean_count
  end if

  if(noahmp%state%snow_level_ice%daily_mean_flag) then
    noahmp%state%snow_level_ice%daily_mean = noahmp%state%snow_level_ice%daily_mean + &
                                             noahmp%state%snow_level_ice%data
    if(end_of_day) noahmp%state%snow_level_ice%daily_mean = &
                     noahmp%state%snow_level_ice%daily_mean / daily_mean_count
  end if

  if(noahmp%state%snow_level_liquid%daily_mean_flag) then
    noahmp%state%snow_level_liquid%daily_mean = noahmp%state%snow_level_liquid%daily_mean + &
                                                noahmp%state%snow_level_liquid%data
    if(end_of_day) noahmp%state%snow_level_liquid%daily_mean = &
                     noahmp%state%snow_level_liquid%daily_mean / daily_mean_count
  end if

  if(noahmp%state%canopy_liquid%daily_mean_flag) then
    noahmp%state%canopy_liquid%daily_mean = noahmp%state%canopy_liquid%daily_mean + &
                                            noahmp%state%canopy_liquid%data
    if(end_of_day) noahmp%state%canopy_liquid%daily_mean = &
                     noahmp%state%canopy_liquid%daily_mean / daily_mean_count
  end if

  if(noahmp%state%canopy_ice%daily_mean_flag) then
    noahmp%state%canopy_ice%daily_mean = noahmp%state%canopy_ice%daily_mean + &
                                         noahmp%state%canopy_ice%data
    if(end_of_day) noahmp%state%canopy_ice%daily_mean = &
                     noahmp%state%canopy_ice%daily_mean / daily_mean_count
  end if

  if(noahmp%state%aquifer_water%daily_mean_flag) then
    noahmp%state%aquifer_water%daily_mean = noahmp%state%aquifer_water%daily_mean + &
                                            noahmp%state%aquifer_water%data
    if(end_of_day) noahmp%state%aquifer_water%daily_mean = &
                     noahmp%state%aquifer_water%daily_mean / daily_mean_count
  end if

  if(noahmp%state%saturated_water%daily_mean_flag) then
    noahmp%state%saturated_water%daily_mean = noahmp%state%saturated_water%daily_mean + &
                                              noahmp%state%saturated_water%data
    if(end_of_day) noahmp%state%saturated_water%daily_mean = &
                     noahmp%state%saturated_water%daily_mean / daily_mean_count
  end if

  if(noahmp%state%lake_water%daily_mean_flag) then
    noahmp%state%lake_water%daily_mean = noahmp%state%lake_water%daily_mean + &
                                         noahmp%state%lake_water%data
    if(end_of_day) noahmp%state%lake_water%daily_mean = &
                     noahmp%state%lake_water%daily_mean / daily_mean_count
  end if

  if(noahmp%state%soil_moisture_wtd%daily_mean_flag) then
    noahmp%state%soil_moisture_wtd%daily_mean = noahmp%state%soil_moisture_wtd%daily_mean + &
                                                noahmp%state%soil_moisture_wtd%data
    if(end_of_day) noahmp%state%soil_moisture_wtd%daily_mean = &
                     noahmp%state%soil_moisture_wtd%daily_mean / daily_mean_count
  end if

  if(noahmp%state%eq_soil_water_vol%daily_mean_flag) then
    noahmp%state%eq_soil_water_vol%daily_mean = noahmp%state%eq_soil_water_vol%daily_mean + &
                                                noahmp%state%eq_soil_water_vol%data
    if(end_of_day) noahmp%state%eq_soil_water_vol%daily_mean = &
                     noahmp%state%eq_soil_water_vol%daily_mean / daily_mean_count
  end if

  if(noahmp%state%leaf_carbon%daily_mean_flag) then
    noahmp%state%leaf_carbon%daily_mean = noahmp%state%leaf_carbon%daily_mean + &
                                          noahmp%state%leaf_carbon%data
    if(end_of_day) noahmp%state%leaf_carbon%daily_mean = &
                     noahmp%state%leaf_carbon%daily_mean / daily_mean_count
  end if

  if(noahmp%state%root_carbon%daily_mean_flag) then
    noahmp%state%root_carbon%daily_mean = noahmp%state%root_carbon%daily_mean + &
                                          noahmp%state%root_carbon%data
    if(end_of_day) noahmp%state%root_carbon%daily_mean = &
                     noahmp%state%root_carbon%daily_mean / daily_mean_count
  end if

  if(noahmp%state%stem_carbon%daily_mean_flag) then
    noahmp%state%stem_carbon%daily_mean = noahmp%state%stem_carbon%daily_mean + &
                                          noahmp%state%stem_carbon%data
    if(end_of_day) noahmp%state%stem_carbon%daily_mean = &
                     noahmp%state%stem_carbon%daily_mean / daily_mean_count
  end if

  if(noahmp%state%wood_carbon%daily_mean_flag) then
    noahmp%state%wood_carbon%daily_mean = noahmp%state%wood_carbon%daily_mean + &
                                          noahmp%state%wood_carbon%data
    if(end_of_day) noahmp%state%wood_carbon%daily_mean = &
                     noahmp%state%wood_carbon%daily_mean / daily_mean_count
  end if

  if(noahmp%state%soil_carbon_stable%daily_mean_flag) then
    noahmp%state%soil_carbon_stable%daily_mean = noahmp%state%soil_carbon_stable%daily_mean + &
                                                 noahmp%state%soil_carbon_stable%data
    if(end_of_day) noahmp%state%soil_carbon_stable%daily_mean = &
                     noahmp%state%soil_carbon_stable%daily_mean / daily_mean_count
  end if

  if(noahmp%state%soil_carbon_fast%daily_mean_flag) then
    noahmp%state%soil_carbon_fast%daily_mean = noahmp%state%soil_carbon_fast%daily_mean + &
                                               noahmp%state%soil_carbon_fast%data
    if(end_of_day) noahmp%state%soil_carbon_fast%daily_mean = &
                     noahmp%state%soil_carbon_fast%daily_mean / daily_mean_count
  end if

  if(noahmp%state%grain_carbon%daily_mean_flag) then
    noahmp%state%grain_carbon%daily_mean = noahmp%state%grain_carbon%daily_mean + &
                                           noahmp%state%grain_carbon%data
    if(end_of_day) noahmp%state%grain_carbon%daily_mean = &
                     noahmp%state%grain_carbon%daily_mean / daily_mean_count
  end if

  if(noahmp%state%foliage_nitrogen%daily_mean_flag) then
    noahmp%state%foliage_nitrogen%daily_mean = noahmp%state%foliage_nitrogen%daily_mean + &
                                               noahmp%state%foliage_nitrogen%data
    if(end_of_day) noahmp%state%foliage_nitrogen%daily_mean = &
                     noahmp%state%foliage_nitrogen%daily_mean / daily_mean_count
  end if

  if(noahmp%state%snow_water_equiv_old%daily_mean_flag) then
    noahmp%state%snow_water_equiv_old%daily_mean = noahmp%state%snow_water_equiv_old%daily_mean + &
                                                   noahmp%state%snow_water_equiv_old%data
    if(end_of_day) noahmp%state%snow_water_equiv_old%daily_mean = &
                     noahmp%state%snow_water_equiv_old%daily_mean / daily_mean_count
  end if

  if(noahmp%state%snow_depth%daily_mean_flag) then
    noahmp%state%snow_depth%daily_mean = noahmp%state%snow_depth%daily_mean + &
                                         noahmp%state%snow_depth%data
    if(end_of_day) noahmp%state%snow_depth%daily_mean = &
                     noahmp%state%snow_depth%daily_mean / daily_mean_count
  end if

  if(noahmp%state%snow_age%daily_mean_flag) then
    noahmp%state%snow_age%daily_mean = noahmp%state%snow_age%daily_mean + &
                                       noahmp%state%snow_age%data
    if(end_of_day) noahmp%state%snow_age%daily_mean = &
                     noahmp%state%snow_age%daily_mean / daily_mean_count
  end if

! Begin noahmp%flux variables

  if(noahmp%flux%sw_absorbed_total%daily_mean_flag) then
    noahmp%flux%sw_absorbed_total%daily_mean = noahmp%flux%sw_absorbed_total%daily_mean + &
                                               noahmp%flux%sw_absorbed_total%data
    if(end_of_day) noahmp%flux%sw_absorbed_total%daily_mean = &
                     noahmp%flux%sw_absorbed_total%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%sw_reflected_total%daily_mean_flag) then
    noahmp%flux%sw_reflected_total%daily_mean = noahmp%flux%sw_reflected_total%daily_mean + &
                                                noahmp%flux%sw_reflected_total%data
    if(end_of_day) noahmp%flux%sw_reflected_total%daily_mean = &
                     noahmp%flux%sw_reflected_total%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%lw_absorbed_total%daily_mean_flag) then
    noahmp%flux%lw_absorbed_total%daily_mean = noahmp%flux%lw_absorbed_total%daily_mean + &
                                               noahmp%flux%lw_absorbed_total%data
    if(end_of_day) noahmp%flux%lw_absorbed_total%daily_mean = &
                     noahmp%flux%lw_absorbed_total%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%sensible_heat_total%daily_mean_flag) then
    noahmp%flux%sensible_heat_total%daily_mean = noahmp%flux%sensible_heat_total%daily_mean + &
                                                 noahmp%flux%sensible_heat_total%data
    if(end_of_day) noahmp%flux%sensible_heat_total%daily_mean = &
                     noahmp%flux%sensible_heat_total%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%transpiration_heat%daily_mean_flag) then
    noahmp%flux%transpiration_heat%daily_mean = noahmp%flux%transpiration_heat%daily_mean + &
                                                noahmp%flux%transpiration_heat%data
    if(end_of_day) noahmp%flux%transpiration_heat%daily_mean = &
                     noahmp%flux%transpiration_heat%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%latent_heat_canopy%daily_mean_flag) then
    noahmp%flux%latent_heat_canopy%daily_mean = noahmp%flux%latent_heat_canopy%daily_mean + &
                                                noahmp%flux%latent_heat_canopy%data
    if(end_of_day) noahmp%flux%latent_heat_canopy%daily_mean = &
                     noahmp%flux%latent_heat_canopy%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%latent_heat_ground%daily_mean_flag) then
    noahmp%flux%latent_heat_ground%daily_mean = noahmp%flux%latent_heat_ground%daily_mean + &
                                                noahmp%flux%latent_heat_ground%data
    if(end_of_day) noahmp%flux%latent_heat_ground%daily_mean = &
                     noahmp%flux%latent_heat_ground%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%latent_heat_total%daily_mean_flag) then
    noahmp%flux%latent_heat_total%daily_mean = noahmp%flux%latent_heat_total%daily_mean + &
                                               noahmp%flux%latent_heat_total%data
    if(end_of_day) noahmp%flux%latent_heat_total%daily_mean = &
                     noahmp%flux%latent_heat_total%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%ground_heat_total%daily_mean_flag) then
    noahmp%flux%ground_heat_total%daily_mean = noahmp%flux%ground_heat_total%daily_mean + &
                                               noahmp%flux%ground_heat_total%data
    if(end_of_day) noahmp%flux%ground_heat_total%daily_mean = &
                     noahmp%flux%ground_heat_total%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%precip_adv_heat_total%daily_mean_flag) then
    noahmp%flux%precip_adv_heat_total%daily_mean = noahmp%flux%precip_adv_heat_total%daily_mean + &
                                                   noahmp%flux%precip_adv_heat_total%data
    if(end_of_day) noahmp%flux%precip_adv_heat_total%daily_mean = &
                     noahmp%flux%precip_adv_heat_total%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%sw_absorbed_veg%daily_mean_flag) then
    noahmp%flux%sw_absorbed_veg%daily_mean = noahmp%flux%sw_absorbed_veg%daily_mean + &
                                             noahmp%flux%sw_absorbed_veg%data
    if(end_of_day) noahmp%flux%sw_absorbed_veg%daily_mean = &
                     noahmp%flux%sw_absorbed_veg%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%sw_absorbed_ground%daily_mean_flag) then
    noahmp%flux%sw_absorbed_ground%daily_mean = noahmp%flux%sw_absorbed_ground%daily_mean + &
                                                noahmp%flux%sw_absorbed_ground%data
    if(end_of_day) noahmp%flux%sw_absorbed_ground%daily_mean = &
                     noahmp%flux%sw_absorbed_ground%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%lw_absorbed_grd_veg%daily_mean_flag) then
    noahmp%flux%lw_absorbed_grd_veg%daily_mean = noahmp%flux%lw_absorbed_grd_veg%daily_mean + &
                                                 noahmp%flux%lw_absorbed_grd_veg%data
    if(end_of_day) noahmp%flux%lw_absorbed_grd_veg%daily_mean = &
                     noahmp%flux%lw_absorbed_grd_veg%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%lw_absorbed_leaf%daily_mean_flag) then
    noahmp%flux%lw_absorbed_leaf%daily_mean = noahmp%flux%lw_absorbed_leaf%daily_mean + &
                                              noahmp%flux%lw_absorbed_leaf%data
    if(end_of_day) noahmp%flux%lw_absorbed_leaf%daily_mean = &
                     noahmp%flux%lw_absorbed_leaf%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%lw_absorbed_grd_bare%daily_mean_flag) then
    noahmp%flux%lw_absorbed_grd_bare%daily_mean = noahmp%flux%lw_absorbed_grd_bare%daily_mean + &
                                                  noahmp%flux%lw_absorbed_grd_bare%data
    if(end_of_day) noahmp%flux%lw_absorbed_grd_bare%daily_mean = &
                     noahmp%flux%lw_absorbed_grd_bare%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%sensible_heat_grd_veg%daily_mean_flag) then
    noahmp%flux%sensible_heat_grd_veg%daily_mean = noahmp%flux%sensible_heat_grd_veg%daily_mean + &
                                                   noahmp%flux%sensible_heat_grd_veg%data
    if(end_of_day) noahmp%flux%sensible_heat_grd_veg%daily_mean = &
                     noahmp%flux%sensible_heat_grd_veg%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%sensible_heat_leaf%daily_mean_flag) then
    noahmp%flux%sensible_heat_leaf%daily_mean = noahmp%flux%sensible_heat_leaf%daily_mean + &
                                                noahmp%flux%sensible_heat_leaf%data
    if(end_of_day) noahmp%flux%sensible_heat_leaf%daily_mean = &
                     noahmp%flux%sensible_heat_leaf%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%sensible_heat_grd_bar%daily_mean_flag) then
    noahmp%flux%sensible_heat_grd_bar%daily_mean = noahmp%flux%sensible_heat_grd_bar%daily_mean + &
                                                   noahmp%flux%sensible_heat_grd_bar%data
    if(end_of_day) noahmp%flux%sensible_heat_grd_bar%daily_mean = &
                     noahmp%flux%sensible_heat_grd_bar%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%latent_heat_trans%daily_mean_flag) then
    noahmp%flux%latent_heat_trans%daily_mean = noahmp%flux%latent_heat_trans%daily_mean + &
                                               noahmp%flux%latent_heat_trans%data
    if(end_of_day) noahmp%flux%latent_heat_trans%daily_mean = &
                     noahmp%flux%latent_heat_trans%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%latent_heat_leaf%daily_mean_flag) then
    noahmp%flux%latent_heat_leaf%daily_mean = noahmp%flux%latent_heat_leaf%daily_mean + &
                                              noahmp%flux%latent_heat_leaf%data
    if(end_of_day) noahmp%flux%latent_heat_leaf%daily_mean = &
                     noahmp%flux%latent_heat_leaf%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%latent_heat_grd_veg%daily_mean_flag) then
    noahmp%flux%latent_heat_grd_veg%daily_mean = noahmp%flux%latent_heat_grd_veg%daily_mean + &
                                                 noahmp%flux%latent_heat_grd_veg%data
    if(end_of_day) noahmp%flux%latent_heat_grd_veg%daily_mean = &
                     noahmp%flux%latent_heat_grd_veg%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%latent_heat_grd_bare%daily_mean_flag) then
    noahmp%flux%latent_heat_grd_bare%daily_mean = noahmp%flux%latent_heat_grd_bare%daily_mean + &
                                                  noahmp%flux%latent_heat_grd_bare%data
    if(end_of_day) noahmp%flux%latent_heat_grd_bare%daily_mean = &
                     noahmp%flux%latent_heat_grd_bare%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%snow_sublimation%daily_mean_flag) then
    noahmp%flux%snow_sublimation%daily_mean = noahmp%flux%snow_sublimation%daily_mean + &
                                              noahmp%flux%snow_sublimation%data
    if(end_of_day) noahmp%flux%snow_sublimation%daily_mean = &
                     noahmp%flux%snow_sublimation%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%ground_heat_veg%daily_mean_flag) then
    noahmp%flux%ground_heat_veg%daily_mean = noahmp%flux%ground_heat_veg%daily_mean + &
                                             noahmp%flux%ground_heat_veg%data
    if(end_of_day) noahmp%flux%ground_heat_veg%daily_mean = &
                     noahmp%flux%ground_heat_veg%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%ground_heat_bare%daily_mean_flag) then
    noahmp%flux%ground_heat_bare%daily_mean = noahmp%flux%ground_heat_bare%daily_mean + &
                                              noahmp%flux%ground_heat_bare%data
    if(end_of_day) noahmp%flux%ground_heat_bare%daily_mean = &
                     noahmp%flux%ground_heat_bare%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%precip_adv_heat_veg%daily_mean_flag) then
    noahmp%flux%precip_adv_heat_veg%daily_mean = noahmp%flux%precip_adv_heat_veg%daily_mean + &
                                                 noahmp%flux%precip_adv_heat_veg%data
    if(end_of_day) noahmp%flux%precip_adv_heat_veg%daily_mean = &
                     noahmp%flux%precip_adv_heat_veg%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%precip_adv_heat_grd_v%daily_mean_flag) then
    noahmp%flux%precip_adv_heat_grd_v%daily_mean = noahmp%flux%precip_adv_heat_grd_v%daily_mean + &
                                                   noahmp%flux%precip_adv_heat_grd_v%data
    if(end_of_day) noahmp%flux%precip_adv_heat_grd_v%daily_mean = &
                     noahmp%flux%precip_adv_heat_grd_v%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%precip_adv_heat_grd_b%daily_mean_flag) then
    noahmp%flux%precip_adv_heat_grd_b%daily_mean = noahmp%flux%precip_adv_heat_grd_b%daily_mean + &
                                                   noahmp%flux%precip_adv_heat_grd_b%data
    if(end_of_day) noahmp%flux%precip_adv_heat_grd_b%daily_mean = &
                     noahmp%flux%precip_adv_heat_grd_b%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%transpiration%daily_mean_flag) then
    noahmp%flux%transpiration%daily_mean = noahmp%flux%transpiration%daily_mean + &
                                           noahmp%flux%transpiration%data
    if(end_of_day) noahmp%flux%transpiration%daily_mean = &
                     noahmp%flux%transpiration%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%evaporation_canopy%daily_mean_flag) then
    noahmp%flux%evaporation_canopy%daily_mean = noahmp%flux%evaporation_canopy%daily_mean + &
                                                noahmp%flux%evaporation_canopy%data
    if(end_of_day) noahmp%flux%evaporation_canopy%daily_mean = &
                     noahmp%flux%evaporation_canopy%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%evaporation_soil%daily_mean_flag) then
    noahmp%flux%evaporation_soil%daily_mean = noahmp%flux%evaporation_soil%daily_mean + &
                                              noahmp%flux%evaporation_soil%data
    if(end_of_day) noahmp%flux%evaporation_soil%daily_mean = &
                     noahmp%flux%evaporation_soil%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%runoff_surface%daily_mean_flag) then
    noahmp%flux%runoff_surface%daily_mean = noahmp%flux%runoff_surface%daily_mean + &
                                            noahmp%flux%runoff_surface%data
    if(end_of_day) noahmp%flux%runoff_surface%daily_mean = &
                     noahmp%flux%runoff_surface%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%runoff_baseflow%daily_mean_flag) then
    noahmp%flux%runoff_baseflow%daily_mean = noahmp%flux%runoff_baseflow%daily_mean + &
                                             noahmp%flux%runoff_baseflow%data
    if(end_of_day) noahmp%flux%runoff_baseflow%daily_mean = &
                     noahmp%flux%runoff_baseflow%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%snowmelt_out%daily_mean_flag) then
    noahmp%flux%snowmelt_out%daily_mean = noahmp%flux%snowmelt_out%daily_mean + &
                                          noahmp%flux%snowmelt_out%data
    if(end_of_day) noahmp%flux%snowmelt_out%daily_mean = &
                     noahmp%flux%snowmelt_out%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%snowmelt_shallow%daily_mean_flag) then
    noahmp%flux%snowmelt_shallow%daily_mean = noahmp%flux%snowmelt_shallow%daily_mean + &
                                              noahmp%flux%snowmelt_shallow%data
    if(end_of_day) noahmp%flux%snowmelt_shallow%daily_mean = &
                     noahmp%flux%snowmelt_shallow%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%snowmelt_shallow_1%daily_mean_flag) then
    noahmp%flux%snowmelt_shallow_1%daily_mean = noahmp%flux%snowmelt_shallow_1%daily_mean + &
                                                noahmp%flux%snowmelt_shallow_1%data
    if(end_of_day) noahmp%flux%snowmelt_shallow_1%daily_mean = &
                     noahmp%flux%snowmelt_shallow_1%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%snowmelt_shallow_2%daily_mean_flag) then
    noahmp%flux%snowmelt_shallow_2%daily_mean = noahmp%flux%snowmelt_shallow_2%daily_mean + &
                                                noahmp%flux%snowmelt_shallow_2%data
    if(end_of_day) noahmp%flux%snowmelt_shallow_2%daily_mean = &
                     noahmp%flux%snowmelt_shallow_2%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%deep_recharge%daily_mean_flag) then
    noahmp%flux%deep_recharge%daily_mean = noahmp%flux%deep_recharge%daily_mean + &
                                           noahmp%flux%deep_recharge%data
    if(end_of_day) noahmp%flux%deep_recharge%daily_mean = &
                     noahmp%flux%deep_recharge%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%recharge%daily_mean_flag) then
    noahmp%flux%recharge%daily_mean = noahmp%flux%recharge%daily_mean + &
                                      noahmp%flux%recharge%data
    if(end_of_day) noahmp%flux%recharge%daily_mean = &
                     noahmp%flux%recharge%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%par_absorbed%daily_mean_flag) then
    noahmp%flux%par_absorbed%daily_mean = noahmp%flux%par_absorbed%daily_mean + &
                                          noahmp%flux%par_absorbed%data
    if(end_of_day) noahmp%flux%par_absorbed%daily_mean = &
                     noahmp%flux%par_absorbed%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%photosynthesis%daily_mean_flag) then
    noahmp%flux%photosynthesis%daily_mean = noahmp%flux%photosynthesis%daily_mean + &
                                            noahmp%flux%photosynthesis%data
    if(end_of_day) noahmp%flux%photosynthesis%daily_mean = &
                     noahmp%flux%photosynthesis%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%net_eco_exchange%daily_mean_flag) then
    noahmp%flux%net_eco_exchange%daily_mean = noahmp%flux%net_eco_exchange%daily_mean + &
                                              noahmp%flux%net_eco_exchange%data
    if(end_of_day) noahmp%flux%net_eco_exchange%daily_mean = &
                     noahmp%flux%net_eco_exchange%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%global_prim_prod%daily_mean_flag) then
    noahmp%flux%global_prim_prod%daily_mean = noahmp%flux%global_prim_prod%daily_mean + &
                                              noahmp%flux%global_prim_prod%data
    if(end_of_day) noahmp%flux%global_prim_prod%daily_mean = &
                     noahmp%flux%global_prim_prod%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%net_prim_prod%daily_mean_flag) then
    noahmp%flux%net_prim_prod%daily_mean = noahmp%flux%net_prim_prod%daily_mean + &
                                           noahmp%flux%net_prim_prod%data
    if(end_of_day) noahmp%flux%net_prim_prod%daily_mean = &
                     noahmp%flux%net_prim_prod%daily_mean / daily_mean_count
  end if

  if(noahmp%flux%canopy_heat_storage%daily_mean_flag) then
    noahmp%flux%canopy_heat_storage%daily_mean = noahmp%flux%canopy_heat_storage%daily_mean + &
                                                 noahmp%flux%canopy_heat_storage%data
    if(end_of_day) noahmp%flux%canopy_heat_storage%daily_mean = &
                     noahmp%flux%canopy_heat_storage%daily_mean / daily_mean_count
  end if

  end subroutine DailyMeanNoahMP

  subroutine MonthlyMeanNoahMP(noahmp)
  
  use ufsLandNoahMPType
  
  type(noahmp_type)    :: noahmp

  logical :: end_of_month
  integer :: monthly_mean_count

! Begin noahmp%static variables

  if(noahmp%static%vegetation_category%monthly_mean_flag) then
    noahmp%static%vegetation_category%monthly_mean = noahmp%static%vegetation_category%monthly_mean + &
                                                     noahmp%static%vegetation_category%data
    if(end_of_month) noahmp%static%vegetation_category%monthly_mean = &
                     noahmp%static%vegetation_category%monthly_mean / monthly_mean_count
  end if

  if(noahmp%static%soil_category%monthly_mean_flag) then
    noahmp%static%soil_category%monthly_mean = noahmp%static%soil_category%monthly_mean + &
                                               noahmp%static%soil_category%data
    if(end_of_month) noahmp%static%soil_category%monthly_mean = &
                     noahmp%static%soil_category%monthly_mean / monthly_mean_count
  end if

  if(noahmp%static%slope_category%monthly_mean_flag) then
    noahmp%static%slope_category%monthly_mean = noahmp%static%slope_category%monthly_mean + &
                                                noahmp%static%slope_category%data
    if(end_of_month) noahmp%static%slope_category%monthly_mean = &
                     noahmp%static%slope_category%monthly_mean / monthly_mean_count
  end if

  if(noahmp%static%soil_interface_depth%monthly_mean_flag) then
    noahmp%static%soil_interface_depth%monthly_mean = noahmp%static%soil_interface_depth%monthly_mean + &
                                                      noahmp%static%soil_interface_depth%data
    if(end_of_month) noahmp%static%soil_interface_depth%monthly_mean = &
                     noahmp%static%soil_interface_depth%monthly_mean / monthly_mean_count
  end if

  if(noahmp%static%ice_flag%monthly_mean_flag) then
    noahmp%static%ice_flag%monthly_mean = noahmp%static%ice_flag%monthly_mean + &
                                          noahmp%static%ice_flag%data
    if(end_of_month) noahmp%static%ice_flag%monthly_mean = &
                     noahmp%static%ice_flag%monthly_mean / monthly_mean_count
  end if

  if(noahmp%static%surface_type%monthly_mean_flag) then
    noahmp%static%surface_type%monthly_mean = noahmp%static%surface_type%monthly_mean + &
                                              noahmp%static%surface_type%data
    if(end_of_month) noahmp%static%surface_type%monthly_mean = &
                     noahmp%static%surface_type%monthly_mean / monthly_mean_count
  end if

  if(noahmp%static%crop_type%monthly_mean_flag) then
    noahmp%static%crop_type%monthly_mean = noahmp%static%crop_type%monthly_mean + &
                                           noahmp%static%crop_type%data
    if(end_of_month) noahmp%static%crop_type%monthly_mean = &
                     noahmp%static%crop_type%monthly_mean / monthly_mean_count
  end if

  if(noahmp%static%temperature_soil_bot%monthly_mean_flag) then
    noahmp%static%temperature_soil_bot%monthly_mean = noahmp%static%temperature_soil_bot%monthly_mean + &
                                                      noahmp%static%temperature_soil_bot%data
    if(end_of_month) noahmp%static%temperature_soil_bot%monthly_mean = &
                     noahmp%static%temperature_soil_bot%monthly_mean / monthly_mean_count
  end if

! Begin noahmp%model variables

  if(noahmp%model%latitude%monthly_mean_flag) then
    noahmp%model%latitude%monthly_mean = noahmp%model%latitude%monthly_mean + &
                                         noahmp%model%latitude%data
    if(end_of_month) noahmp%model%latitude%monthly_mean = &
                     noahmp%model%latitude%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%cosine_zenith%monthly_mean_flag) then
    noahmp%model%cosine_zenith%monthly_mean = noahmp%model%cosine_zenith%monthly_mean + &
                                              noahmp%model%cosine_zenith%data
    if(end_of_month) noahmp%model%cosine_zenith%monthly_mean = &
                     noahmp%model%cosine_zenith%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%forcing_height%monthly_mean_flag) then
    noahmp%model%forcing_height%monthly_mean = noahmp%model%forcing_height%monthly_mean + &
                                               noahmp%model%forcing_height%data
    if(end_of_month) noahmp%model%forcing_height%monthly_mean = &
                     noahmp%model%forcing_height%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%vegetation_fraction%monthly_mean_flag) then
    noahmp%model%vegetation_fraction%monthly_mean = noahmp%model%vegetation_fraction%monthly_mean + &
                                                    noahmp%model%vegetation_fraction%data
    if(end_of_month) noahmp%model%vegetation_fraction%monthly_mean = &
                     noahmp%model%vegetation_fraction%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%max_vegetation_frac%monthly_mean_flag) then
    noahmp%model%max_vegetation_frac%monthly_mean = noahmp%model%max_vegetation_frac%monthly_mean + &
                                                    noahmp%model%max_vegetation_frac%data
    if(end_of_month) noahmp%model%max_vegetation_frac%monthly_mean = &
                     noahmp%model%max_vegetation_frac%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%active_snow_levels%monthly_mean_flag) then
    noahmp%model%active_snow_levels%monthly_mean = noahmp%model%active_snow_levels%monthly_mean + &
                                                   noahmp%model%active_snow_levels%data
    if(end_of_month) noahmp%model%active_snow_levels%monthly_mean = &
                     noahmp%model%active_snow_levels%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%interface_depth%monthly_mean_flag) then
    noahmp%model%interface_depth%monthly_mean = noahmp%model%interface_depth%monthly_mean + &
                                                noahmp%model%interface_depth%data
    if(end_of_month) noahmp%model%interface_depth%monthly_mean = &
                     noahmp%model%interface_depth%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%snow_soil_thickness%monthly_mean_flag) then
    noahmp%model%snow_soil_thickness%monthly_mean = noahmp%model%snow_soil_thickness%monthly_mean + &
                                                    noahmp%model%snow_soil_thickness%data
    if(end_of_month) noahmp%model%snow_soil_thickness%monthly_mean = &
                     noahmp%model%snow_soil_thickness%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%leaf_area_index%monthly_mean_flag) then
    noahmp%model%leaf_area_index%monthly_mean = noahmp%model%leaf_area_index%monthly_mean + &
                                                noahmp%model%leaf_area_index%data
    if(end_of_month) noahmp%model%leaf_area_index%monthly_mean = &
                     noahmp%model%leaf_area_index%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%stem_area_index%monthly_mean_flag) then
    noahmp%model%stem_area_index%monthly_mean = noahmp%model%stem_area_index%monthly_mean + &
                                                noahmp%model%stem_area_index%data
    if(end_of_month) noahmp%model%stem_area_index%monthly_mean = &
                     noahmp%model%stem_area_index%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%growing_deg_days%monthly_mean_flag) then
    noahmp%model%growing_deg_days%monthly_mean = noahmp%model%growing_deg_days%monthly_mean + &
                                                 noahmp%model%growing_deg_days%data
    if(end_of_month) noahmp%model%growing_deg_days%monthly_mean = &
                     noahmp%model%growing_deg_days%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%plant_growth_stage%monthly_mean_flag) then
    noahmp%model%plant_growth_stage%monthly_mean = noahmp%model%plant_growth_stage%monthly_mean + &
                                                   noahmp%model%plant_growth_stage%data
    if(end_of_month) noahmp%model%plant_growth_stage%monthly_mean = &
                     noahmp%model%plant_growth_stage%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%cm_noahmp%monthly_mean_flag) then
    noahmp%model%cm_noahmp%monthly_mean = noahmp%model%cm_noahmp%monthly_mean + &
                                          noahmp%model%cm_noahmp%data
    if(end_of_month) noahmp%model%cm_noahmp%monthly_mean = &
                     noahmp%model%cm_noahmp%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%ch_noahmp%monthly_mean_flag) then
    noahmp%model%ch_noahmp%monthly_mean = noahmp%model%ch_noahmp%monthly_mean + &
                                          noahmp%model%ch_noahmp%data
    if(end_of_month) noahmp%model%ch_noahmp%monthly_mean = &
                     noahmp%model%ch_noahmp%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%ch_vegetated%monthly_mean_flag) then
    noahmp%model%ch_vegetated%monthly_mean = noahmp%model%ch_vegetated%monthly_mean + &
                                             noahmp%model%ch_vegetated%data
    if(end_of_month) noahmp%model%ch_vegetated%monthly_mean = &
                     noahmp%model%ch_vegetated%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%ch_bare_ground%monthly_mean_flag) then
    noahmp%model%ch_bare_ground%monthly_mean = noahmp%model%ch_bare_ground%monthly_mean + &
                                               noahmp%model%ch_bare_ground%data
    if(end_of_month) noahmp%model%ch_bare_ground%monthly_mean = &
                     noahmp%model%ch_bare_ground%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%ch_leaf%monthly_mean_flag) then
    noahmp%model%ch_leaf%monthly_mean = noahmp%model%ch_leaf%monthly_mean + &
                                        noahmp%model%ch_leaf%data
    if(end_of_month) noahmp%model%ch_leaf%monthly_mean = &
                     noahmp%model%ch_leaf%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%ch_below_canopy%monthly_mean_flag) then
    noahmp%model%ch_below_canopy%monthly_mean = noahmp%model%ch_below_canopy%monthly_mean + &
                                                noahmp%model%ch_below_canopy%data
    if(end_of_month) noahmp%model%ch_below_canopy%monthly_mean = &
                     noahmp%model%ch_below_canopy%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%ch_vegetated_2m%monthly_mean_flag) then
    noahmp%model%ch_vegetated_2m%monthly_mean = noahmp%model%ch_vegetated_2m%monthly_mean + &
                                                noahmp%model%ch_vegetated_2m%data
    if(end_of_month) noahmp%model%ch_vegetated_2m%monthly_mean = &
                     noahmp%model%ch_vegetated_2m%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%ch_bare_ground_2m%monthly_mean_flag) then
    noahmp%model%ch_bare_ground_2m%monthly_mean = noahmp%model%ch_bare_ground_2m%monthly_mean + &
                                                  noahmp%model%ch_bare_ground_2m%data
    if(end_of_month) noahmp%model%ch_bare_ground_2m%monthly_mean = &
                     noahmp%model%ch_bare_ground_2m%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%friction_velocity%monthly_mean_flag) then
    noahmp%model%friction_velocity%monthly_mean = noahmp%model%friction_velocity%monthly_mean + &
                                                  noahmp%model%friction_velocity%data
    if(end_of_month) noahmp%model%friction_velocity%monthly_mean = &
                     noahmp%model%friction_velocity%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%rs_sunlit%monthly_mean_flag) then
    noahmp%model%rs_sunlit%monthly_mean = noahmp%model%rs_sunlit%monthly_mean + &
                                          noahmp%model%rs_sunlit%data
    if(end_of_month) noahmp%model%rs_sunlit%monthly_mean = &
                     noahmp%model%rs_sunlit%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%rs_shaded%monthly_mean_flag) then
    noahmp%model%rs_shaded%monthly_mean = noahmp%model%rs_shaded%monthly_mean + &
                                          noahmp%model%rs_shaded%data
    if(end_of_month) noahmp%model%rs_shaded%monthly_mean = &
                     noahmp%model%rs_shaded%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%leaf_air_resistance%monthly_mean_flag) then
    noahmp%model%leaf_air_resistance%monthly_mean = noahmp%model%leaf_air_resistance%monthly_mean + &
                                                    noahmp%model%leaf_air_resistance%data
    if(end_of_month) noahmp%model%leaf_air_resistance%monthly_mean = &
                     noahmp%model%leaf_air_resistance%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%pbl_height%monthly_mean_flag) then
    noahmp%model%pbl_height%monthly_mean = noahmp%model%pbl_height%monthly_mean + &
                                           noahmp%model%pbl_height%data
    if(end_of_month) noahmp%model%pbl_height%monthly_mean = &
                     noahmp%model%pbl_height%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%mo_length_inverse%monthly_mean_flag) then
    noahmp%model%mo_length_inverse%monthly_mean = noahmp%model%mo_length_inverse%monthly_mean + &
                                                  noahmp%model%mo_length_inverse%data
    if(end_of_month) noahmp%model%mo_length_inverse%monthly_mean = &
                     noahmp%model%mo_length_inverse%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%heat_flux_multiplier%monthly_mean_flag) then
    noahmp%model%heat_flux_multiplier%monthly_mean = noahmp%model%heat_flux_multiplier%monthly_mean + &
                                                     noahmp%model%heat_flux_multiplier%data
    if(end_of_month) noahmp%model%heat_flux_multiplier%monthly_mean = &
                     noahmp%model%heat_flux_multiplier%monthly_mean / monthly_mean_count
  end if

  if(noahmp%model%moisture_flux_multiplier%monthly_mean_flag) then
    noahmp%model%moisture_flux_multiplier%monthly_mean = noahmp%model%moisture_flux_multiplier%monthly_mean + &
                                                         noahmp%model%moisture_flux_multiplier%data
    if(end_of_month) noahmp%model%moisture_flux_multiplier%monthly_mean = &
                     noahmp%model%moisture_flux_multiplier%monthly_mean / monthly_mean_count
  end if

! Begin noahmp%forcing variables

  if(noahmp%forcing%temperature_forcing%monthly_mean_flag) then
    noahmp%forcing%temperature_forcing%monthly_mean = noahmp%forcing%temperature_forcing%monthly_mean + &
                                                      noahmp%forcing%temperature_forcing%data
    if(end_of_month) noahmp%forcing%temperature_forcing%monthly_mean = &
                     noahmp%forcing%temperature_forcing%monthly_mean / monthly_mean_count
  end if

  if(noahmp%forcing%specific_humidity_forcing%monthly_mean_flag) then
    noahmp%forcing%specific_humidity_forcing%monthly_mean = noahmp%forcing%specific_humidity_forcing%monthly_mean + &
                                                            noahmp%forcing%specific_humidity_forcing%data
    if(end_of_month) noahmp%forcing%specific_humidity_forcing%monthly_mean = &
                     noahmp%forcing%specific_humidity_forcing%monthly_mean / monthly_mean_count
  end if

  if(noahmp%forcing%surface_pressure_forcing%monthly_mean_flag) then
    noahmp%forcing%surface_pressure_forcing%monthly_mean = noahmp%forcing%surface_pressure_forcing%monthly_mean + &
                                                           noahmp%forcing%surface_pressure_forcing%data
    if(end_of_month) noahmp%forcing%surface_pressure_forcing%monthly_mean = &
                     noahmp%forcing%surface_pressure_forcing%monthly_mean / monthly_mean_count
  end if

  if(noahmp%forcing%wind_speed_forcing%monthly_mean_flag) then
    noahmp%forcing%wind_speed_forcing%monthly_mean = noahmp%forcing%wind_speed_forcing%monthly_mean + &
                                                     noahmp%forcing%wind_speed_forcing%data
    if(end_of_month) noahmp%forcing%wind_speed_forcing%monthly_mean = &
                     noahmp%forcing%wind_speed_forcing%monthly_mean / monthly_mean_count
  end if

  if(noahmp%forcing%downward_longwave_forcing%monthly_mean_flag) then
    noahmp%forcing%downward_longwave_forcing%monthly_mean = noahmp%forcing%downward_longwave_forcing%monthly_mean + &
                                                            noahmp%forcing%downward_longwave_forcing%data
    if(end_of_month) noahmp%forcing%downward_longwave_forcing%monthly_mean = &
                     noahmp%forcing%downward_longwave_forcing%monthly_mean / monthly_mean_count
  end if

  if(noahmp%forcing%downward_shortwave_forcing%monthly_mean_flag) then
    noahmp%forcing%downward_shortwave_forcing%monthly_mean = noahmp%forcing%downward_shortwave_forcing%monthly_mean + &
                                                             noahmp%forcing%downward_shortwave_forcing%data
    if(end_of_month) noahmp%forcing%downward_shortwave_forcing%monthly_mean = &
                     noahmp%forcing%downward_shortwave_forcing%monthly_mean / monthly_mean_count
  end if

  if(noahmp%forcing%precipitation_forcing%monthly_mean_flag) then
    noahmp%forcing%precipitation_forcing%monthly_mean = noahmp%forcing%precipitation_forcing%monthly_mean + &
                                                        noahmp%forcing%precipitation_forcing%data
    if(end_of_month) noahmp%forcing%precipitation_forcing%monthly_mean = &
                     noahmp%forcing%precipitation_forcing%monthly_mean / monthly_mean_count
  end if

  if(noahmp%forcing%precip_convective%monthly_mean_flag) then
    noahmp%forcing%precip_convective%monthly_mean = noahmp%forcing%precip_convective%monthly_mean + &
                                                    noahmp%forcing%precip_convective%data
    if(end_of_month) noahmp%forcing%precip_convective%monthly_mean = &
                     noahmp%forcing%precip_convective%monthly_mean / monthly_mean_count
  end if

  if(noahmp%forcing%precip_non_convective%monthly_mean_flag) then
    noahmp%forcing%precip_non_convective%monthly_mean = noahmp%forcing%precip_non_convective%monthly_mean + &
                                                        noahmp%forcing%precip_non_convective%data
    if(end_of_month) noahmp%forcing%precip_non_convective%monthly_mean = &
                     noahmp%forcing%precip_non_convective%monthly_mean / monthly_mean_count
  end if

  if(noahmp%forcing%precip_snow%monthly_mean_flag) then
    noahmp%forcing%precip_snow%monthly_mean = noahmp%forcing%precip_snow%monthly_mean + &
                                              noahmp%forcing%precip_snow%data
    if(end_of_month) noahmp%forcing%precip_snow%monthly_mean = &
                     noahmp%forcing%precip_snow%monthly_mean / monthly_mean_count
  end if

  if(noahmp%forcing%precip_graupel%monthly_mean_flag) then
    noahmp%forcing%precip_graupel%monthly_mean = noahmp%forcing%precip_graupel%monthly_mean + &
                                                 noahmp%forcing%precip_graupel%data
    if(end_of_month) noahmp%forcing%precip_graupel%monthly_mean = &
                     noahmp%forcing%precip_graupel%monthly_mean / monthly_mean_count
  end if

  if(noahmp%forcing%precip_hail%monthly_mean_flag) then
    noahmp%forcing%precip_hail%monthly_mean = noahmp%forcing%precip_hail%monthly_mean + &
                                              noahmp%forcing%precip_hail%data
    if(end_of_month) noahmp%forcing%precip_hail%monthly_mean = &
                     noahmp%forcing%precip_hail%monthly_mean / monthly_mean_count
  end if

  if(noahmp%forcing%snowfall%monthly_mean_flag) then
    noahmp%forcing%snowfall%monthly_mean = noahmp%forcing%snowfall%monthly_mean + &
                                           noahmp%forcing%snowfall%data
    if(end_of_month) noahmp%forcing%snowfall%monthly_mean = &
                     noahmp%forcing%snowfall%monthly_mean / monthly_mean_count
  end if

  if(noahmp%forcing%rainfall%monthly_mean_flag) then
    noahmp%forcing%rainfall%monthly_mean = noahmp%forcing%rainfall%monthly_mean + &
                                           noahmp%forcing%rainfall%data
    if(end_of_month) noahmp%forcing%rainfall%monthly_mean = &
                     noahmp%forcing%rainfall%monthly_mean / monthly_mean_count
  end if

! Begin noahmp%diag variables

  if(noahmp%diag%z0_total%monthly_mean_flag) then
    noahmp%diag%z0_total%monthly_mean = noahmp%diag%z0_total%monthly_mean + &
                                        noahmp%diag%z0_total%data
    if(end_of_month) noahmp%diag%z0_total%monthly_mean = &
                     noahmp%diag%z0_total%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%z0h_total%monthly_mean_flag) then
    noahmp%diag%z0h_total%monthly_mean = noahmp%diag%z0h_total%monthly_mean + &
                                         noahmp%diag%z0h_total%data
    if(end_of_month) noahmp%diag%z0h_total%monthly_mean = &
                     noahmp%diag%z0h_total%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%albedo_total%monthly_mean_flag) then
    noahmp%diag%albedo_total%monthly_mean = noahmp%diag%albedo_total%monthly_mean + &
                                            noahmp%diag%albedo_total%data
    if(end_of_month) noahmp%diag%albedo_total%monthly_mean = &
                     noahmp%diag%albedo_total%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%albedo_direct%monthly_mean_flag) then
    noahmp%diag%albedo_direct%monthly_mean = noahmp%diag%albedo_direct%monthly_mean + &
                                             noahmp%diag%albedo_direct%data
    if(end_of_month) noahmp%diag%albedo_direct%monthly_mean = &
                     noahmp%diag%albedo_direct%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%albedo_diffuse%monthly_mean_flag) then
    noahmp%diag%albedo_diffuse%monthly_mean = noahmp%diag%albedo_diffuse%monthly_mean + &
                                              noahmp%diag%albedo_diffuse%data
    if(end_of_month) noahmp%diag%albedo_diffuse%monthly_mean = &
                     noahmp%diag%albedo_diffuse%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%albedo_direct_snow%monthly_mean_flag) then
    noahmp%diag%albedo_direct_snow%monthly_mean = noahmp%diag%albedo_direct_snow%monthly_mean + &
                                                  noahmp%diag%albedo_direct_snow%data
    if(end_of_month) noahmp%diag%albedo_direct_snow%monthly_mean = &
                     noahmp%diag%albedo_direct_snow%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%albedo_diffuse_snow%monthly_mean_flag) then
    noahmp%diag%albedo_diffuse_snow%monthly_mean = noahmp%diag%albedo_diffuse_snow%monthly_mean + &
                                                   noahmp%diag%albedo_diffuse_snow%data
    if(end_of_month) noahmp%diag%albedo_diffuse_snow%monthly_mean = &
                     noahmp%diag%albedo_diffuse_snow%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%emissivity_total%monthly_mean_flag) then
    noahmp%diag%emissivity_total%monthly_mean = noahmp%diag%emissivity_total%monthly_mean + &
                                                noahmp%diag%emissivity_total%data
    if(end_of_month) noahmp%diag%emissivity_total%monthly_mean = &
                     noahmp%diag%emissivity_total%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%canopy_gap_fraction%monthly_mean_flag) then
    noahmp%diag%canopy_gap_fraction%monthly_mean = noahmp%diag%canopy_gap_fraction%monthly_mean + &
                                                   noahmp%diag%canopy_gap_fraction%data
    if(end_of_month) noahmp%diag%canopy_gap_fraction%monthly_mean = &
                     noahmp%diag%canopy_gap_fraction%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%incanopy_gap_fraction%monthly_mean_flag) then
    noahmp%diag%incanopy_gap_fraction%monthly_mean = noahmp%diag%incanopy_gap_fraction%monthly_mean + &
                                                     noahmp%diag%incanopy_gap_fraction%data
    if(end_of_month) noahmp%diag%incanopy_gap_fraction%monthly_mean = &
                     noahmp%diag%incanopy_gap_fraction%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%precip_frozen_frac%monthly_mean_flag) then
    noahmp%diag%precip_frozen_frac%monthly_mean = noahmp%diag%precip_frozen_frac%monthly_mean + &
                                                  noahmp%diag%precip_frozen_frac%data
    if(end_of_month) noahmp%diag%precip_frozen_frac%monthly_mean = &
                     noahmp%diag%precip_frozen_frac%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%snow_cover_fraction%monthly_mean_flag) then
    noahmp%diag%snow_cover_fraction%monthly_mean = noahmp%diag%snow_cover_fraction%monthly_mean + &
                                                   noahmp%diag%snow_cover_fraction%data
    if(end_of_month) noahmp%diag%snow_cover_fraction%monthly_mean = &
                     noahmp%diag%snow_cover_fraction%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%canopy_wet_fraction%monthly_mean_flag) then
    noahmp%diag%canopy_wet_fraction%monthly_mean = noahmp%diag%canopy_wet_fraction%monthly_mean + &
                                                   noahmp%diag%canopy_wet_fraction%data
    if(end_of_month) noahmp%diag%canopy_wet_fraction%monthly_mean = &
                     noahmp%diag%canopy_wet_fraction%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%canopy_water%monthly_mean_flag) then
    noahmp%diag%canopy_water%monthly_mean = noahmp%diag%canopy_water%monthly_mean + &
                                            noahmp%diag%canopy_water%data
    if(end_of_month) noahmp%diag%canopy_water%monthly_mean = &
                     noahmp%diag%canopy_water%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%depth_water_table%monthly_mean_flag) then
    noahmp%diag%depth_water_table%monthly_mean = noahmp%diag%depth_water_table%monthly_mean + &
                                                 noahmp%diag%depth_water_table%data
    if(end_of_month) noahmp%diag%depth_water_table%monthly_mean = &
                     noahmp%diag%depth_water_table%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%lai_sunlit%monthly_mean_flag) then
    noahmp%diag%lai_sunlit%monthly_mean = noahmp%diag%lai_sunlit%monthly_mean + &
                                          noahmp%diag%lai_sunlit%data
    if(end_of_month) noahmp%diag%lai_sunlit%monthly_mean = &
                     noahmp%diag%lai_sunlit%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%lai_shaded%monthly_mean_flag) then
    noahmp%diag%lai_shaded%monthly_mean = noahmp%diag%lai_shaded%monthly_mean + &
                                          noahmp%diag%lai_shaded%data
    if(end_of_month) noahmp%diag%lai_shaded%monthly_mean = &
                     noahmp%diag%lai_shaded%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%snow_ice_frac_old%monthly_mean_flag) then
    noahmp%diag%snow_ice_frac_old%monthly_mean = noahmp%diag%snow_ice_frac_old%monthly_mean + &
                                                 noahmp%diag%snow_ice_frac_old%data
    if(end_of_month) noahmp%diag%snow_ice_frac_old%monthly_mean = &
                     noahmp%diag%snow_ice_frac_old%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%snow_albedo_old%monthly_mean_flag) then
    noahmp%diag%snow_albedo_old%monthly_mean = noahmp%diag%snow_albedo_old%monthly_mean + &
                                               noahmp%diag%snow_albedo_old%data
    if(end_of_month) noahmp%diag%snow_albedo_old%monthly_mean = &
                     noahmp%diag%snow_albedo_old%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%evaporation_potential%monthly_mean_flag) then
    noahmp%diag%evaporation_potential%monthly_mean = noahmp%diag%evaporation_potential%monthly_mean + &
                                                     noahmp%diag%evaporation_potential%data
    if(end_of_month) noahmp%diag%evaporation_potential%monthly_mean = &
                     noahmp%diag%evaporation_potential%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%soil_moisture_total%monthly_mean_flag) then
    noahmp%diag%soil_moisture_total%monthly_mean = noahmp%diag%soil_moisture_total%monthly_mean + &
                                                   noahmp%diag%soil_moisture_total%data
    if(end_of_month) noahmp%diag%soil_moisture_total%monthly_mean = &
                     noahmp%diag%soil_moisture_total%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%temperature_veg_2m%monthly_mean_flag) then
    noahmp%diag%temperature_veg_2m%monthly_mean = noahmp%diag%temperature_veg_2m%monthly_mean + &
                                                  noahmp%diag%temperature_veg_2m%data
    if(end_of_month) noahmp%diag%temperature_veg_2m%monthly_mean = &
                     noahmp%diag%temperature_veg_2m%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%temperature_bare_2m%monthly_mean_flag) then
    noahmp%diag%temperature_bare_2m%monthly_mean = noahmp%diag%temperature_bare_2m%monthly_mean + &
                                                   noahmp%diag%temperature_bare_2m%data
    if(end_of_month) noahmp%diag%temperature_bare_2m%monthly_mean = &
                     noahmp%diag%temperature_bare_2m%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%temperature_2m%monthly_mean_flag) then
    noahmp%diag%temperature_2m%monthly_mean = noahmp%diag%temperature_2m%monthly_mean + &
                                              noahmp%diag%temperature_2m%data
    if(end_of_month) noahmp%diag%temperature_2m%monthly_mean = &
                     noahmp%diag%temperature_2m%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%spec_humidity_veg_2m%monthly_mean_flag) then
    noahmp%diag%spec_humidity_veg_2m%monthly_mean = noahmp%diag%spec_humidity_veg_2m%monthly_mean + &
                                                    noahmp%diag%spec_humidity_veg_2m%data
    if(end_of_month) noahmp%diag%spec_humidity_veg_2m%monthly_mean = &
                     noahmp%diag%spec_humidity_veg_2m%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%spec_humidity_bare_2m%monthly_mean_flag) then
    noahmp%diag%spec_humidity_bare_2m%monthly_mean = noahmp%diag%spec_humidity_bare_2m%monthly_mean + &
                                                     noahmp%diag%spec_humidity_bare_2m%data
    if(end_of_month) noahmp%diag%spec_humidity_bare_2m%monthly_mean = &
                     noahmp%diag%spec_humidity_bare_2m%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%spec_humidity_2m%monthly_mean_flag) then
    noahmp%diag%spec_humidity_2m%monthly_mean = noahmp%diag%spec_humidity_2m%monthly_mean + &
                                                noahmp%diag%spec_humidity_2m%data
    if(end_of_month) noahmp%diag%spec_humidity_2m%monthly_mean = &
                     noahmp%diag%spec_humidity_2m%monthly_mean / monthly_mean_count
  end if

  if(noahmp%diag%spec_humidity_surface%monthly_mean_flag) then
    noahmp%diag%spec_humidity_surface%monthly_mean = noahmp%diag%spec_humidity_surface%monthly_mean + &
                                                     noahmp%diag%spec_humidity_surface%data
    if(end_of_month) noahmp%diag%spec_humidity_surface%monthly_mean = &
                     noahmp%diag%spec_humidity_surface%monthly_mean / monthly_mean_count
  end if

! Begin noahmp%state variables

  if(noahmp%state%temperature_soil%monthly_mean_flag) then
    noahmp%state%temperature_soil%monthly_mean = noahmp%state%temperature_soil%monthly_mean + &
                                                 noahmp%state%temperature_soil%data
    if(end_of_month) noahmp%state%temperature_soil%monthly_mean = &
                     noahmp%state%temperature_soil%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%temperature_snow%monthly_mean_flag) then
    noahmp%state%temperature_snow%monthly_mean = noahmp%state%temperature_snow%monthly_mean + &
                                                 noahmp%state%temperature_snow%data
    if(end_of_month) noahmp%state%temperature_snow%monthly_mean = &
                     noahmp%state%temperature_snow%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%temperature_canopy_air%monthly_mean_flag) then
    noahmp%state%temperature_canopy_air%monthly_mean = noahmp%state%temperature_canopy_air%monthly_mean + &
                                                       noahmp%state%temperature_canopy_air%data
    if(end_of_month) noahmp%state%temperature_canopy_air%monthly_mean = &
                     noahmp%state%temperature_canopy_air%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%temperature_radiative%monthly_mean_flag) then
    noahmp%state%temperature_radiative%monthly_mean = noahmp%state%temperature_radiative%monthly_mean + &
                                                      noahmp%state%temperature_radiative%data
    if(end_of_month) noahmp%state%temperature_radiative%monthly_mean = &
                     noahmp%state%temperature_radiative%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%temperature_leaf%monthly_mean_flag) then
    noahmp%state%temperature_leaf%monthly_mean = noahmp%state%temperature_leaf%monthly_mean + &
                                                 noahmp%state%temperature_leaf%data
    if(end_of_month) noahmp%state%temperature_leaf%monthly_mean = &
                     noahmp%state%temperature_leaf%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%temperature_ground%monthly_mean_flag) then
    noahmp%state%temperature_ground%monthly_mean = noahmp%state%temperature_ground%monthly_mean + &
                                                   noahmp%state%temperature_ground%data
    if(end_of_month) noahmp%state%temperature_ground%monthly_mean = &
                     noahmp%state%temperature_ground%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%temperature_bare_grd%monthly_mean_flag) then
    noahmp%state%temperature_bare_grd%monthly_mean = noahmp%state%temperature_bare_grd%monthly_mean + &
                                                     noahmp%state%temperature_bare_grd%data
    if(end_of_month) noahmp%state%temperature_bare_grd%monthly_mean = &
                     noahmp%state%temperature_bare_grd%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%temperature_veg_grd%monthly_mean_flag) then
    noahmp%state%temperature_veg_grd%monthly_mean = noahmp%state%temperature_veg_grd%monthly_mean + &
                                                    noahmp%state%temperature_veg_grd%data
    if(end_of_month) noahmp%state%temperature_veg_grd%monthly_mean = &
                     noahmp%state%temperature_veg_grd%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%vapor_pres_canopy_air%monthly_mean_flag) then
    noahmp%state%vapor_pres_canopy_air%monthly_mean = noahmp%state%vapor_pres_canopy_air%monthly_mean + &
                                                      noahmp%state%vapor_pres_canopy_air%data
    if(end_of_month) noahmp%state%vapor_pres_canopy_air%monthly_mean = &
                     noahmp%state%vapor_pres_canopy_air%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%soil_liquid_vol%monthly_mean_flag) then
    noahmp%state%soil_liquid_vol%monthly_mean = noahmp%state%soil_liquid_vol%monthly_mean + &
                                                noahmp%state%soil_liquid_vol%data
    if(end_of_month) noahmp%state%soil_liquid_vol%monthly_mean = &
                     noahmp%state%soil_liquid_vol%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%soil_moisture_vol%monthly_mean_flag) then
    noahmp%state%soil_moisture_vol%monthly_mean = noahmp%state%soil_moisture_vol%monthly_mean + &
                                                  noahmp%state%soil_moisture_vol%data
    if(end_of_month) noahmp%state%soil_moisture_vol%monthly_mean = &
                     noahmp%state%soil_moisture_vol%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%snow_water_equiv%monthly_mean_flag) then
    noahmp%state%snow_water_equiv%monthly_mean = noahmp%state%snow_water_equiv%monthly_mean + &
                                                 noahmp%state%snow_water_equiv%data
    if(end_of_month) noahmp%state%snow_water_equiv%monthly_mean = &
                     noahmp%state%snow_water_equiv%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%snow_level_ice%monthly_mean_flag) then
    noahmp%state%snow_level_ice%monthly_mean = noahmp%state%snow_level_ice%monthly_mean + &
                                               noahmp%state%snow_level_ice%data
    if(end_of_month) noahmp%state%snow_level_ice%monthly_mean = &
                     noahmp%state%snow_level_ice%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%snow_level_liquid%monthly_mean_flag) then
    noahmp%state%snow_level_liquid%monthly_mean = noahmp%state%snow_level_liquid%monthly_mean + &
                                                  noahmp%state%snow_level_liquid%data
    if(end_of_month) noahmp%state%snow_level_liquid%monthly_mean = &
                     noahmp%state%snow_level_liquid%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%canopy_liquid%monthly_mean_flag) then
    noahmp%state%canopy_liquid%monthly_mean = noahmp%state%canopy_liquid%monthly_mean + &
                                              noahmp%state%canopy_liquid%data
    if(end_of_month) noahmp%state%canopy_liquid%monthly_mean = &
                     noahmp%state%canopy_liquid%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%canopy_ice%monthly_mean_flag) then
    noahmp%state%canopy_ice%monthly_mean = noahmp%state%canopy_ice%monthly_mean + &
                                           noahmp%state%canopy_ice%data
    if(end_of_month) noahmp%state%canopy_ice%monthly_mean = &
                     noahmp%state%canopy_ice%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%aquifer_water%monthly_mean_flag) then
    noahmp%state%aquifer_water%monthly_mean = noahmp%state%aquifer_water%monthly_mean + &
                                              noahmp%state%aquifer_water%data
    if(end_of_month) noahmp%state%aquifer_water%monthly_mean = &
                     noahmp%state%aquifer_water%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%saturated_water%monthly_mean_flag) then
    noahmp%state%saturated_water%monthly_mean = noahmp%state%saturated_water%monthly_mean + &
                                                noahmp%state%saturated_water%data
    if(end_of_month) noahmp%state%saturated_water%monthly_mean = &
                     noahmp%state%saturated_water%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%lake_water%monthly_mean_flag) then
    noahmp%state%lake_water%monthly_mean = noahmp%state%lake_water%monthly_mean + &
                                           noahmp%state%lake_water%data
    if(end_of_month) noahmp%state%lake_water%monthly_mean = &
                     noahmp%state%lake_water%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%soil_moisture_wtd%monthly_mean_flag) then
    noahmp%state%soil_moisture_wtd%monthly_mean = noahmp%state%soil_moisture_wtd%monthly_mean + &
                                                  noahmp%state%soil_moisture_wtd%data
    if(end_of_month) noahmp%state%soil_moisture_wtd%monthly_mean = &
                     noahmp%state%soil_moisture_wtd%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%eq_soil_water_vol%monthly_mean_flag) then
    noahmp%state%eq_soil_water_vol%monthly_mean = noahmp%state%eq_soil_water_vol%monthly_mean + &
                                                  noahmp%state%eq_soil_water_vol%data
    if(end_of_month) noahmp%state%eq_soil_water_vol%monthly_mean = &
                     noahmp%state%eq_soil_water_vol%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%leaf_carbon%monthly_mean_flag) then
    noahmp%state%leaf_carbon%monthly_mean = noahmp%state%leaf_carbon%monthly_mean + &
                                            noahmp%state%leaf_carbon%data
    if(end_of_month) noahmp%state%leaf_carbon%monthly_mean = &
                     noahmp%state%leaf_carbon%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%root_carbon%monthly_mean_flag) then
    noahmp%state%root_carbon%monthly_mean = noahmp%state%root_carbon%monthly_mean + &
                                            noahmp%state%root_carbon%data
    if(end_of_month) noahmp%state%root_carbon%monthly_mean = &
                     noahmp%state%root_carbon%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%stem_carbon%monthly_mean_flag) then
    noahmp%state%stem_carbon%monthly_mean = noahmp%state%stem_carbon%monthly_mean + &
                                            noahmp%state%stem_carbon%data
    if(end_of_month) noahmp%state%stem_carbon%monthly_mean = &
                     noahmp%state%stem_carbon%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%wood_carbon%monthly_mean_flag) then
    noahmp%state%wood_carbon%monthly_mean = noahmp%state%wood_carbon%monthly_mean + &
                                            noahmp%state%wood_carbon%data
    if(end_of_month) noahmp%state%wood_carbon%monthly_mean = &
                     noahmp%state%wood_carbon%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%soil_carbon_stable%monthly_mean_flag) then
    noahmp%state%soil_carbon_stable%monthly_mean = noahmp%state%soil_carbon_stable%monthly_mean + &
                                                   noahmp%state%soil_carbon_stable%data
    if(end_of_month) noahmp%state%soil_carbon_stable%monthly_mean = &
                     noahmp%state%soil_carbon_stable%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%soil_carbon_fast%monthly_mean_flag) then
    noahmp%state%soil_carbon_fast%monthly_mean = noahmp%state%soil_carbon_fast%monthly_mean + &
                                                 noahmp%state%soil_carbon_fast%data
    if(end_of_month) noahmp%state%soil_carbon_fast%monthly_mean = &
                     noahmp%state%soil_carbon_fast%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%grain_carbon%monthly_mean_flag) then
    noahmp%state%grain_carbon%monthly_mean = noahmp%state%grain_carbon%monthly_mean + &
                                             noahmp%state%grain_carbon%data
    if(end_of_month) noahmp%state%grain_carbon%monthly_mean = &
                     noahmp%state%grain_carbon%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%foliage_nitrogen%monthly_mean_flag) then
    noahmp%state%foliage_nitrogen%monthly_mean = noahmp%state%foliage_nitrogen%monthly_mean + &
                                                 noahmp%state%foliage_nitrogen%data
    if(end_of_month) noahmp%state%foliage_nitrogen%monthly_mean = &
                     noahmp%state%foliage_nitrogen%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%snow_water_equiv_old%monthly_mean_flag) then
    noahmp%state%snow_water_equiv_old%monthly_mean = noahmp%state%snow_water_equiv_old%monthly_mean + &
                                                     noahmp%state%snow_water_equiv_old%data
    if(end_of_month) noahmp%state%snow_water_equiv_old%monthly_mean = &
                     noahmp%state%snow_water_equiv_old%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%snow_depth%monthly_mean_flag) then
    noahmp%state%snow_depth%monthly_mean = noahmp%state%snow_depth%monthly_mean + &
                                           noahmp%state%snow_depth%data
    if(end_of_month) noahmp%state%snow_depth%monthly_mean = &
                     noahmp%state%snow_depth%monthly_mean / monthly_mean_count
  end if

  if(noahmp%state%snow_age%monthly_mean_flag) then
    noahmp%state%snow_age%monthly_mean = noahmp%state%snow_age%monthly_mean + &
                                         noahmp%state%snow_age%data
    if(end_of_month) noahmp%state%snow_age%monthly_mean = &
                     noahmp%state%snow_age%monthly_mean / monthly_mean_count
  end if

! Begin noahmp%flux variables

  if(noahmp%flux%sw_absorbed_total%monthly_mean_flag) then
    noahmp%flux%sw_absorbed_total%monthly_mean = noahmp%flux%sw_absorbed_total%monthly_mean + &
                                                 noahmp%flux%sw_absorbed_total%data
    if(end_of_month) noahmp%flux%sw_absorbed_total%monthly_mean = &
                     noahmp%flux%sw_absorbed_total%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%sw_reflected_total%monthly_mean_flag) then
    noahmp%flux%sw_reflected_total%monthly_mean = noahmp%flux%sw_reflected_total%monthly_mean + &
                                                  noahmp%flux%sw_reflected_total%data
    if(end_of_month) noahmp%flux%sw_reflected_total%monthly_mean = &
                     noahmp%flux%sw_reflected_total%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%lw_absorbed_total%monthly_mean_flag) then
    noahmp%flux%lw_absorbed_total%monthly_mean = noahmp%flux%lw_absorbed_total%monthly_mean + &
                                                 noahmp%flux%lw_absorbed_total%data
    if(end_of_month) noahmp%flux%lw_absorbed_total%monthly_mean = &
                     noahmp%flux%lw_absorbed_total%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%sensible_heat_total%monthly_mean_flag) then
    noahmp%flux%sensible_heat_total%monthly_mean = noahmp%flux%sensible_heat_total%monthly_mean + &
                                                   noahmp%flux%sensible_heat_total%data
    if(end_of_month) noahmp%flux%sensible_heat_total%monthly_mean = &
                     noahmp%flux%sensible_heat_total%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%transpiration_heat%monthly_mean_flag) then
    noahmp%flux%transpiration_heat%monthly_mean = noahmp%flux%transpiration_heat%monthly_mean + &
                                                  noahmp%flux%transpiration_heat%data
    if(end_of_month) noahmp%flux%transpiration_heat%monthly_mean = &
                     noahmp%flux%transpiration_heat%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%latent_heat_canopy%monthly_mean_flag) then
    noahmp%flux%latent_heat_canopy%monthly_mean = noahmp%flux%latent_heat_canopy%monthly_mean + &
                                                  noahmp%flux%latent_heat_canopy%data
    if(end_of_month) noahmp%flux%latent_heat_canopy%monthly_mean = &
                     noahmp%flux%latent_heat_canopy%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%latent_heat_ground%monthly_mean_flag) then
    noahmp%flux%latent_heat_ground%monthly_mean = noahmp%flux%latent_heat_ground%monthly_mean + &
                                                  noahmp%flux%latent_heat_ground%data
    if(end_of_month) noahmp%flux%latent_heat_ground%monthly_mean = &
                     noahmp%flux%latent_heat_ground%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%latent_heat_total%monthly_mean_flag) then
    noahmp%flux%latent_heat_total%monthly_mean = noahmp%flux%latent_heat_total%monthly_mean + &
                                                 noahmp%flux%latent_heat_total%data
    if(end_of_month) noahmp%flux%latent_heat_total%monthly_mean = &
                     noahmp%flux%latent_heat_total%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%ground_heat_total%monthly_mean_flag) then
    noahmp%flux%ground_heat_total%monthly_mean = noahmp%flux%ground_heat_total%monthly_mean + &
                                                 noahmp%flux%ground_heat_total%data
    if(end_of_month) noahmp%flux%ground_heat_total%monthly_mean = &
                     noahmp%flux%ground_heat_total%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%precip_adv_heat_total%monthly_mean_flag) then
    noahmp%flux%precip_adv_heat_total%monthly_mean = noahmp%flux%precip_adv_heat_total%monthly_mean + &
                                                     noahmp%flux%precip_adv_heat_total%data
    if(end_of_month) noahmp%flux%precip_adv_heat_total%monthly_mean = &
                     noahmp%flux%precip_adv_heat_total%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%sw_absorbed_veg%monthly_mean_flag) then
    noahmp%flux%sw_absorbed_veg%monthly_mean = noahmp%flux%sw_absorbed_veg%monthly_mean + &
                                               noahmp%flux%sw_absorbed_veg%data
    if(end_of_month) noahmp%flux%sw_absorbed_veg%monthly_mean = &
                     noahmp%flux%sw_absorbed_veg%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%sw_absorbed_ground%monthly_mean_flag) then
    noahmp%flux%sw_absorbed_ground%monthly_mean = noahmp%flux%sw_absorbed_ground%monthly_mean + &
                                                  noahmp%flux%sw_absorbed_ground%data
    if(end_of_month) noahmp%flux%sw_absorbed_ground%monthly_mean = &
                     noahmp%flux%sw_absorbed_ground%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%lw_absorbed_grd_veg%monthly_mean_flag) then
    noahmp%flux%lw_absorbed_grd_veg%monthly_mean = noahmp%flux%lw_absorbed_grd_veg%monthly_mean + &
                                                   noahmp%flux%lw_absorbed_grd_veg%data
    if(end_of_month) noahmp%flux%lw_absorbed_grd_veg%monthly_mean = &
                     noahmp%flux%lw_absorbed_grd_veg%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%lw_absorbed_leaf%monthly_mean_flag) then
    noahmp%flux%lw_absorbed_leaf%monthly_mean = noahmp%flux%lw_absorbed_leaf%monthly_mean + &
                                                noahmp%flux%lw_absorbed_leaf%data
    if(end_of_month) noahmp%flux%lw_absorbed_leaf%monthly_mean = &
                     noahmp%flux%lw_absorbed_leaf%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%lw_absorbed_grd_bare%monthly_mean_flag) then
    noahmp%flux%lw_absorbed_grd_bare%monthly_mean = noahmp%flux%lw_absorbed_grd_bare%monthly_mean + &
                                                    noahmp%flux%lw_absorbed_grd_bare%data
    if(end_of_month) noahmp%flux%lw_absorbed_grd_bare%monthly_mean = &
                     noahmp%flux%lw_absorbed_grd_bare%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%sensible_heat_grd_veg%monthly_mean_flag) then
    noahmp%flux%sensible_heat_grd_veg%monthly_mean = noahmp%flux%sensible_heat_grd_veg%monthly_mean + &
                                                     noahmp%flux%sensible_heat_grd_veg%data
    if(end_of_month) noahmp%flux%sensible_heat_grd_veg%monthly_mean = &
                     noahmp%flux%sensible_heat_grd_veg%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%sensible_heat_leaf%monthly_mean_flag) then
    noahmp%flux%sensible_heat_leaf%monthly_mean = noahmp%flux%sensible_heat_leaf%monthly_mean + &
                                                  noahmp%flux%sensible_heat_leaf%data
    if(end_of_month) noahmp%flux%sensible_heat_leaf%monthly_mean = &
                     noahmp%flux%sensible_heat_leaf%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%sensible_heat_grd_bar%monthly_mean_flag) then
    noahmp%flux%sensible_heat_grd_bar%monthly_mean = noahmp%flux%sensible_heat_grd_bar%monthly_mean + &
                                                     noahmp%flux%sensible_heat_grd_bar%data
    if(end_of_month) noahmp%flux%sensible_heat_grd_bar%monthly_mean = &
                     noahmp%flux%sensible_heat_grd_bar%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%latent_heat_trans%monthly_mean_flag) then
    noahmp%flux%latent_heat_trans%monthly_mean = noahmp%flux%latent_heat_trans%monthly_mean + &
                                                 noahmp%flux%latent_heat_trans%data
    if(end_of_month) noahmp%flux%latent_heat_trans%monthly_mean = &
                     noahmp%flux%latent_heat_trans%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%latent_heat_leaf%monthly_mean_flag) then
    noahmp%flux%latent_heat_leaf%monthly_mean = noahmp%flux%latent_heat_leaf%monthly_mean + &
                                                noahmp%flux%latent_heat_leaf%data
    if(end_of_month) noahmp%flux%latent_heat_leaf%monthly_mean = &
                     noahmp%flux%latent_heat_leaf%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%latent_heat_grd_veg%monthly_mean_flag) then
    noahmp%flux%latent_heat_grd_veg%monthly_mean = noahmp%flux%latent_heat_grd_veg%monthly_mean + &
                                                   noahmp%flux%latent_heat_grd_veg%data
    if(end_of_month) noahmp%flux%latent_heat_grd_veg%monthly_mean = &
                     noahmp%flux%latent_heat_grd_veg%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%latent_heat_grd_bare%monthly_mean_flag) then
    noahmp%flux%latent_heat_grd_bare%monthly_mean = noahmp%flux%latent_heat_grd_bare%monthly_mean + &
                                                    noahmp%flux%latent_heat_grd_bare%data
    if(end_of_month) noahmp%flux%latent_heat_grd_bare%monthly_mean = &
                     noahmp%flux%latent_heat_grd_bare%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%snow_sublimation%monthly_mean_flag) then
    noahmp%flux%snow_sublimation%monthly_mean = noahmp%flux%snow_sublimation%monthly_mean + &
                                                noahmp%flux%snow_sublimation%data
    if(end_of_month) noahmp%flux%snow_sublimation%monthly_mean = &
                     noahmp%flux%snow_sublimation%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%ground_heat_veg%monthly_mean_flag) then
    noahmp%flux%ground_heat_veg%monthly_mean = noahmp%flux%ground_heat_veg%monthly_mean + &
                                               noahmp%flux%ground_heat_veg%data
    if(end_of_month) noahmp%flux%ground_heat_veg%monthly_mean = &
                     noahmp%flux%ground_heat_veg%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%ground_heat_bare%monthly_mean_flag) then
    noahmp%flux%ground_heat_bare%monthly_mean = noahmp%flux%ground_heat_bare%monthly_mean + &
                                                noahmp%flux%ground_heat_bare%data
    if(end_of_month) noahmp%flux%ground_heat_bare%monthly_mean = &
                     noahmp%flux%ground_heat_bare%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%precip_adv_heat_veg%monthly_mean_flag) then
    noahmp%flux%precip_adv_heat_veg%monthly_mean = noahmp%flux%precip_adv_heat_veg%monthly_mean + &
                                                   noahmp%flux%precip_adv_heat_veg%data
    if(end_of_month) noahmp%flux%precip_adv_heat_veg%monthly_mean = &
                     noahmp%flux%precip_adv_heat_veg%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%precip_adv_heat_grd_v%monthly_mean_flag) then
    noahmp%flux%precip_adv_heat_grd_v%monthly_mean = noahmp%flux%precip_adv_heat_grd_v%monthly_mean + &
                                                     noahmp%flux%precip_adv_heat_grd_v%data
    if(end_of_month) noahmp%flux%precip_adv_heat_grd_v%monthly_mean = &
                     noahmp%flux%precip_adv_heat_grd_v%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%precip_adv_heat_grd_b%monthly_mean_flag) then
    noahmp%flux%precip_adv_heat_grd_b%monthly_mean = noahmp%flux%precip_adv_heat_grd_b%monthly_mean + &
                                                     noahmp%flux%precip_adv_heat_grd_b%data
    if(end_of_month) noahmp%flux%precip_adv_heat_grd_b%monthly_mean = &
                     noahmp%flux%precip_adv_heat_grd_b%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%transpiration%monthly_mean_flag) then
    noahmp%flux%transpiration%monthly_mean = noahmp%flux%transpiration%monthly_mean + &
                                             noahmp%flux%transpiration%data
    if(end_of_month) noahmp%flux%transpiration%monthly_mean = &
                     noahmp%flux%transpiration%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%evaporation_canopy%monthly_mean_flag) then
    noahmp%flux%evaporation_canopy%monthly_mean = noahmp%flux%evaporation_canopy%monthly_mean + &
                                                  noahmp%flux%evaporation_canopy%data
    if(end_of_month) noahmp%flux%evaporation_canopy%monthly_mean = &
                     noahmp%flux%evaporation_canopy%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%evaporation_soil%monthly_mean_flag) then
    noahmp%flux%evaporation_soil%monthly_mean = noahmp%flux%evaporation_soil%monthly_mean + &
                                                noahmp%flux%evaporation_soil%data
    if(end_of_month) noahmp%flux%evaporation_soil%monthly_mean = &
                     noahmp%flux%evaporation_soil%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%runoff_surface%monthly_mean_flag) then
    noahmp%flux%runoff_surface%monthly_mean = noahmp%flux%runoff_surface%monthly_mean + &
                                              noahmp%flux%runoff_surface%data
    if(end_of_month) noahmp%flux%runoff_surface%monthly_mean = &
                     noahmp%flux%runoff_surface%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%runoff_baseflow%monthly_mean_flag) then
    noahmp%flux%runoff_baseflow%monthly_mean = noahmp%flux%runoff_baseflow%monthly_mean + &
                                               noahmp%flux%runoff_baseflow%data
    if(end_of_month) noahmp%flux%runoff_baseflow%monthly_mean = &
                     noahmp%flux%runoff_baseflow%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%snowmelt_out%monthly_mean_flag) then
    noahmp%flux%snowmelt_out%monthly_mean = noahmp%flux%snowmelt_out%monthly_mean + &
                                            noahmp%flux%snowmelt_out%data
    if(end_of_month) noahmp%flux%snowmelt_out%monthly_mean = &
                     noahmp%flux%snowmelt_out%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%snowmelt_shallow%monthly_mean_flag) then
    noahmp%flux%snowmelt_shallow%monthly_mean = noahmp%flux%snowmelt_shallow%monthly_mean + &
                                                noahmp%flux%snowmelt_shallow%data
    if(end_of_month) noahmp%flux%snowmelt_shallow%monthly_mean = &
                     noahmp%flux%snowmelt_shallow%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%snowmelt_shallow_1%monthly_mean_flag) then
    noahmp%flux%snowmelt_shallow_1%monthly_mean = noahmp%flux%snowmelt_shallow_1%monthly_mean + &
                                                  noahmp%flux%snowmelt_shallow_1%data
    if(end_of_month) noahmp%flux%snowmelt_shallow_1%monthly_mean = &
                     noahmp%flux%snowmelt_shallow_1%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%snowmelt_shallow_2%monthly_mean_flag) then
    noahmp%flux%snowmelt_shallow_2%monthly_mean = noahmp%flux%snowmelt_shallow_2%monthly_mean + &
                                                  noahmp%flux%snowmelt_shallow_2%data
    if(end_of_month) noahmp%flux%snowmelt_shallow_2%monthly_mean = &
                     noahmp%flux%snowmelt_shallow_2%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%deep_recharge%monthly_mean_flag) then
    noahmp%flux%deep_recharge%monthly_mean = noahmp%flux%deep_recharge%monthly_mean + &
                                             noahmp%flux%deep_recharge%data
    if(end_of_month) noahmp%flux%deep_recharge%monthly_mean = &
                     noahmp%flux%deep_recharge%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%recharge%monthly_mean_flag) then
    noahmp%flux%recharge%monthly_mean = noahmp%flux%recharge%monthly_mean + &
                                        noahmp%flux%recharge%data
    if(end_of_month) noahmp%flux%recharge%monthly_mean = &
                     noahmp%flux%recharge%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%par_absorbed%monthly_mean_flag) then
    noahmp%flux%par_absorbed%monthly_mean = noahmp%flux%par_absorbed%monthly_mean + &
                                            noahmp%flux%par_absorbed%data
    if(end_of_month) noahmp%flux%par_absorbed%monthly_mean = &
                     noahmp%flux%par_absorbed%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%photosynthesis%monthly_mean_flag) then
    noahmp%flux%photosynthesis%monthly_mean = noahmp%flux%photosynthesis%monthly_mean + &
                                              noahmp%flux%photosynthesis%data
    if(end_of_month) noahmp%flux%photosynthesis%monthly_mean = &
                     noahmp%flux%photosynthesis%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%net_eco_exchange%monthly_mean_flag) then
    noahmp%flux%net_eco_exchange%monthly_mean = noahmp%flux%net_eco_exchange%monthly_mean + &
                                                noahmp%flux%net_eco_exchange%data
    if(end_of_month) noahmp%flux%net_eco_exchange%monthly_mean = &
                     noahmp%flux%net_eco_exchange%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%global_prim_prod%monthly_mean_flag) then
    noahmp%flux%global_prim_prod%monthly_mean = noahmp%flux%global_prim_prod%monthly_mean + &
                                                noahmp%flux%global_prim_prod%data
    if(end_of_month) noahmp%flux%global_prim_prod%monthly_mean = &
                     noahmp%flux%global_prim_prod%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%net_prim_prod%monthly_mean_flag) then
    noahmp%flux%net_prim_prod%monthly_mean = noahmp%flux%net_prim_prod%monthly_mean + &
                                             noahmp%flux%net_prim_prod%data
    if(end_of_month) noahmp%flux%net_prim_prod%monthly_mean = &
                     noahmp%flux%net_prim_prod%monthly_mean / monthly_mean_count
  end if

  if(noahmp%flux%canopy_heat_storage%monthly_mean_flag) then
    noahmp%flux%canopy_heat_storage%monthly_mean = noahmp%flux%canopy_heat_storage%monthly_mean + &
                                                   noahmp%flux%canopy_heat_storage%data
    if(end_of_month) noahmp%flux%canopy_heat_storage%monthly_mean = &
                     noahmp%flux%canopy_heat_storage%monthly_mean / monthly_mean_count
  end if

  end subroutine MonthlyMeanNoahMP

end module ufsLandSpecialOutput
