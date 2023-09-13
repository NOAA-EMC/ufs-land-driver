module ufsLandTimeOutput

  implicit none
  save
  
  integer, parameter, private :: output = 1, restart = 2, daily_mean = 3, monthly_mean = 4,  &
                                 solar_noon = 5

  interface solar_noon_add

    procedure solar_noon_add_int1d
    procedure solar_noon_add_real1d
    procedure solar_noon_add_real2d

  end interface solar_noon_add

contains   

  subroutine solar_noon_add_int1d(indata, solar_noon_hour, inhour, time_type)

    use ufsLandGenericType, only : int1d
    type(int1d)  :: indata
    type(int1d)  :: solar_noon_hour
    integer      :: inhour, time_type

    time_period : select case(time_type)
  
    case( solar_noon )

      if(indata%solar_noon_flag) then
        where(solar_noon_hour%data == inhour) indata%solar_noon = indata%data
      end if
    
    end select time_period

  end subroutine solar_noon_add_int1d

  subroutine solar_noon_add_real1d(indata, solar_noon_hour, inhour, time_type)

    use ufsLandGenericType, only : real1d, int1d
    type(real1d) :: indata
    type(int1d)  :: solar_noon_hour
    integer      :: inhour, time_type

    time_period : select case(time_type)
  
    case( solar_noon )

      if(indata%solar_noon_flag) then
        where(solar_noon_hour%data == inhour) indata%solar_noon = indata%data
      end if
    
    end select time_period

  end subroutine solar_noon_add_real1d

  subroutine solar_noon_add_real2d(indata, solar_noon_hour, inhour, time_type)

    use ufsLandGenericType, only : real2d, int1d
    type(real2d) :: indata
    type(int1d)  :: solar_noon_hour
    integer      :: inhour, time_type, ilev

    time_period : select case(time_type)
  
    case( solar_noon )

      if(indata%solar_noon_flag) then
        do ilev = lbound(indata%data,2), ubound(indata%data,2)
          where(solar_noon_hour%data == inhour) indata%solar_noon(:,ilev) = indata%data(:,ilev)
        end do
      end if
    
    end select time_period

  end subroutine solar_noon_add_real2d

  subroutine SolarNoonNoahMP(noahmp, current_hour)
  
  use ufsLandNoahMPType
  
  type(noahmp_type) :: noahmp
  integer           :: current_hour

! Begin noahmp%static variables

  call solar_noon_add(noahmp%static%vegetation_category         , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%static%soil_category               , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%static%slope_category              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%static%soil_interface_depth        , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%static%ice_flag                    , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%static%surface_type                , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%static%crop_type                   , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%static%temperature_soil_bot        , noahmp%model%solar_noon_hour, current_hour, solar_noon)

! Begin noahmp%model variables

  call solar_noon_add(noahmp%model%latitude                     , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%longitude                    , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%solar_noon_hour              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%cosine_zenith                , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%forcing_height               , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%vegetation_fraction          , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%max_vegetation_frac          , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%active_snow_levels           , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%interface_depth              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%snow_soil_thickness          , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%leaf_area_index              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%stem_area_index              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%growing_deg_days             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%plant_growth_stage           , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%cm_noahmp                    , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%ch_noahmp                    , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%ch_vegetated                 , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%ch_bare_ground               , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%ch_leaf                      , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%ch_below_canopy              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%ch_vegetated_2m              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%ch_bare_ground_2m            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%friction_velocity            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%rs_sunlit                    , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%rs_shaded                    , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%leaf_air_resistance          , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%pbl_height                   , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%mo_length_inverse            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%heat_flux_multiplier         , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%model%moisture_flux_multiplier     , noahmp%model%solar_noon_hour, current_hour, solar_noon)

! Begin noahmp%forcing variables

  call solar_noon_add(noahmp%forcing%temperature_forcing        , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%forcing%specific_humidity_forcing  , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%forcing%surface_pressure_forcing   , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%forcing%wind_speed_forcing         , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%forcing%downward_longwave_forcing  , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%forcing%downward_shortwave_forcing , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%forcing%precipitation_forcing      , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%forcing%precip_convective          , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%forcing%precip_non_convective      , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%forcing%precip_snow                , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%forcing%precip_graupel             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%forcing%precip_hail                , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%forcing%snowfall                   , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%forcing%rainfall                   , noahmp%model%solar_noon_hour, current_hour, solar_noon)

! Begin noahmp%diag variables

  call solar_noon_add(noahmp%diag%z0_total                      , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%z0h_total                     , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%albedo_total                  , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%albedo_direct                 , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%albedo_diffuse                , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%albedo_direct_snow            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%albedo_diffuse_snow           , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%emissivity_total              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%canopy_gap_fraction           , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%incanopy_gap_fraction         , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%precip_frozen_frac            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%snow_cover_fraction           , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%canopy_wet_fraction           , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%canopy_water                  , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%depth_water_table             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%lai_sunlit                    , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%lai_shaded                    , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%snow_ice_frac_old             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%snow_albedo_old               , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%evaporation_potential         , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%soil_moisture_total           , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%temperature_veg_2m            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%temperature_bare_2m           , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%temperature_2m                , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%spec_humidity_veg_2m          , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%spec_humidity_bare_2m         , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%spec_humidity_2m              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%spec_humidity_surface         , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%dewpoint_veg_2m               , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%dewpoint_bare_2m              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%diag%dewpoint_2m                   , noahmp%model%solar_noon_hour, current_hour, solar_noon)

! Begin noahmp%state variables

  call solar_noon_add(noahmp%state%temperature_soil             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%temperature_snow             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%temperature_canopy_air       , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%temperature_radiative        , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%temperature_leaf             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%temperature_ground           , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%temperature_bare_grd         , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%temperature_veg_grd          , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%vapor_pres_canopy_air        , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%soil_liquid_vol              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%soil_moisture_vol            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%snow_water_equiv             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%snow_level_ice               , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%snow_level_liquid            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%canopy_liquid                , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%canopy_ice                   , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%aquifer_water                , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%saturated_water              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%lake_water                   , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%soil_moisture_wtd            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%eq_soil_water_vol            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%leaf_carbon                  , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%root_carbon                  , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%stem_carbon                  , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%wood_carbon                  , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%soil_carbon_stable           , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%soil_carbon_fast             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%grain_carbon                 , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%foliage_nitrogen             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%snow_water_equiv_old         , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%snow_depth                   , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%state%snow_age                     , noahmp%model%solar_noon_hour, current_hour, solar_noon)

! Begin noahmp%flux variables

  call solar_noon_add(noahmp%flux%sw_absorbed_total             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%sw_reflected_total            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%lw_absorbed_total             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%sensible_heat_total           , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%transpiration_heat            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%latent_heat_canopy            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%latent_heat_ground            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%latent_heat_total             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%ground_heat_total             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%precip_adv_heat_total         , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%sw_absorbed_veg               , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%sw_absorbed_ground            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%lw_absorbed_grd_veg           , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%lw_absorbed_leaf              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%lw_absorbed_grd_bare          , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%sensible_heat_grd_veg         , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%sensible_heat_leaf            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%sensible_heat_grd_bar         , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%latent_heat_trans             , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%latent_heat_leaf              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%latent_heat_grd_veg           , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%latent_heat_grd_bare          , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%snow_sublimation              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%ground_heat_veg               , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%ground_heat_bare              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%precip_adv_heat_veg           , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%precip_adv_heat_grd_v         , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%precip_adv_heat_grd_b         , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%transpiration                 , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%evaporation_canopy            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%evaporation_soil              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%runoff_surface                , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%runoff_baseflow               , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%snowmelt_out                  , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%snowmelt_shallow              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%snowmelt_shallow_1            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%snowmelt_shallow_2            , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%deep_recharge                 , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%recharge                      , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%par_absorbed                  , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%photosynthesis                , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%net_eco_exchange              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%global_prim_prod              , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%net_prim_prod                 , noahmp%model%solar_noon_hour, current_hour, solar_noon)
  call solar_noon_add(noahmp%flux%canopy_heat_storage           , noahmp%model%solar_noon_hour, current_hour, solar_noon)

  end subroutine SolarNoonNoahMP

end module ufsLandTimeOutput
