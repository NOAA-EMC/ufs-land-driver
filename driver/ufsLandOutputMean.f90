module ufsLandMeanOutput

  implicit none
  save
  
  integer, parameter, private :: output = 1, restart = 2, daily_mean = 3, monthly_mean = 4,  &
                                 solar_noon = 5, diurnal = 6

  interface accumulate

    procedure accumulate_int1d
    procedure accumulate_real1d
    procedure accumulate_real2d

  end interface accumulate

contains   

  subroutine accumulate_int1d(indata, end_of_period, period_mean_count, mean_type, num_diurnal)

    use ufsLandGenericType, only : int1d
    type(int1d) :: indata
    logical     :: end_of_period
    integer     :: period_mean_count
    integer     :: mean_type
    integer, optional :: num_diurnal
    integer     :: diurnal_time

    time_period : select case(mean_type)
  
    case( daily_mean )

      if(indata%daily_mean_flag) then
        if(period_mean_count == 1) indata%daily_mean = 0
        indata%daily_mean = indata%daily_mean + indata%data
        if(end_of_period) indata%daily_mean = nint(1.0 * indata%daily_mean / period_mean_count)
      end if
    
    case( monthly_mean )

      if(indata%monthly_mean_flag) then
        if(period_mean_count == 1) indata%monthly_mean = 0
        indata%monthly_mean = indata%monthly_mean + indata%data
        if(end_of_period) indata%monthly_mean = nint(1.0 * indata%monthly_mean / period_mean_count)
      end if
    
    case( diurnal )

      if(indata%diurnal_flag) then
        diurnal_time = mod(period_mean_count,num_diurnal)
        if(period_mean_count == 1) indata%diurnal = 0
        indata%diurnal(:,diurnal_time) = indata%diurnal(:,diurnal_time) + indata%data
        if(end_of_period) indata%diurnal = nint(1.0 * indata%diurnal / period_mean_count * num_diurnal)
      end if
    
    end select time_period

  end subroutine accumulate_int1d

  subroutine accumulate_real1d(indata, end_of_period, period_mean_count, mean_type, num_diurnal)

    use ufsLandGenericType, only : real1d
    type(real1d) :: indata
    logical      :: end_of_period
    integer      :: period_mean_count
    integer      :: mean_type
    integer, optional :: num_diurnal
    integer      :: diurnal_time

    time_period : select case(mean_type)
  
    case( daily_mean )

      if(indata%daily_mean_flag) then
        if(period_mean_count == 1) indata%daily_mean = 0.0
        indata%daily_mean = indata%daily_mean + indata%data
        if(end_of_period) indata%daily_mean = indata%daily_mean / period_mean_count
      end if
    
    case( monthly_mean )

      if(indata%monthly_mean_flag) then
        if(period_mean_count == 1) indata%monthly_mean = 0.0
        indata%monthly_mean = indata%monthly_mean + indata%data
        if(end_of_period) indata%monthly_mean = indata%monthly_mean / period_mean_count
      end if
    
    case( diurnal )

      if(indata%diurnal_flag) then
        diurnal_time = mod(period_mean_count-1,num_diurnal) + 1
        if(period_mean_count == 1) indata%diurnal = 0
        indata%diurnal(:,diurnal_time) = indata%diurnal(:,diurnal_time) + indata%data
        if(end_of_period) indata%diurnal = indata%diurnal / period_mean_count * num_diurnal
      end if
    
    end select time_period

  end subroutine accumulate_real1d

  subroutine accumulate_real2d(indata, end_of_period, period_mean_count, mean_type, num_diurnal)

    use ufsLandGenericType, only : real2d
    type(real2d) :: indata
    logical      :: end_of_period
    integer      :: period_mean_count
    integer      :: mean_type
    integer, optional :: num_diurnal
    integer      :: diurnal_time

    time_period : select case(mean_type)
  
    case( daily_mean )

      if(indata%daily_mean_flag) then
        if(period_mean_count == 1) indata%daily_mean = 0.0
        indata%daily_mean = indata%daily_mean + indata%data
        if(end_of_period) indata%daily_mean = indata%daily_mean / period_mean_count
      end if
    
    case( monthly_mean )

      if(indata%monthly_mean_flag) then
        if(period_mean_count == 1) indata%monthly_mean = 0.0
        indata%monthly_mean = indata%monthly_mean + indata%data
        if(end_of_period) indata%monthly_mean = indata%monthly_mean / period_mean_count
      end if
    
    case( diurnal )

      if(indata%diurnal_flag) then
        diurnal_time = mod(period_mean_count,num_diurnal)
        if(period_mean_count == 1) indata%diurnal = 0
        indata%diurnal(:,diurnal_time,:) = indata%diurnal(:,diurnal_time,:) + indata%data
        if(end_of_period) indata%diurnal = indata%diurnal / period_mean_count * num_diurnal
      end if
    
    end select time_period

  end subroutine accumulate_real2d

  subroutine DailyMeanNoahMP(noahmp, end_of_day, daily_mean_count)
  
  use ufsLandNoahMPType
  
  type(noahmp_type) :: noahmp
  logical           :: end_of_day
  integer           :: daily_mean_count

! Begin noahmp%static variables

  call accumulate(noahmp%static%vegetation_category         , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%static%soil_category               , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%static%slope_category              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%static%soil_interface_depth        , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%static%ice_flag                    , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%static%surface_type                , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%static%crop_type                   , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%static%temperature_soil_bot        , end_of_day, daily_mean_count, daily_mean)

! Begin noahmp%model variables

  call accumulate(noahmp%model%latitude                     , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%longitude                    , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%cosine_zenith                , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%forcing_height               , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%vegetation_fraction          , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%max_vegetation_frac          , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%active_snow_levels           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%interface_depth              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%snow_soil_thickness          , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%leaf_area_index              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%stem_area_index              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%growing_deg_days             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%plant_growth_stage           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%cm_noahmp                    , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%ch_noahmp                    , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%ch_vegetated                 , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%ch_bare_ground               , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%ch_leaf                      , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%ch_below_canopy              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%ch_vegetated_2m              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%ch_bare_ground_2m            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%friction_velocity            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%rs_sunlit                    , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%rs_shaded                    , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%leaf_air_resistance          , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%pbl_height                   , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%mo_length_inverse            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%heat_flux_multiplier         , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%model%moisture_flux_multiplier     , end_of_day, daily_mean_count, daily_mean)

! Begin noahmp%forcing variables

  call accumulate(noahmp%forcing%temperature_forcing        , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%forcing%specific_humidity_forcing  , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%forcing%surface_pressure_forcing   , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%forcing%wind_speed_forcing         , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%forcing%downward_longwave_forcing  , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%forcing%downward_shortwave_forcing , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%forcing%precipitation_forcing      , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%forcing%precip_convective          , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%forcing%precip_non_convective      , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%forcing%precip_snow                , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%forcing%precip_graupel             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%forcing%precip_hail                , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%forcing%snowfall                   , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%forcing%rainfall                   , end_of_day, daily_mean_count, daily_mean)

! Begin noahmp%diag variables

  call accumulate(noahmp%diag%z0_total                      , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%z0h_total                     , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%albedo_total                  , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%albedo_direct                 , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%albedo_diffuse                , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%albedo_direct_snow            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%albedo_diffuse_snow           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%emissivity_total              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%canopy_gap_fraction           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%incanopy_gap_fraction         , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%precip_frozen_frac            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%snow_cover_fraction           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%canopy_wet_fraction           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%canopy_water                  , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%depth_water_table             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%lai_sunlit                    , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%lai_shaded                    , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%snow_ice_frac_old             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%snow_albedo_old               , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%evaporation_potential         , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%soil_moisture_total           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%temperature_veg_2m            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%temperature_bare_2m           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%temperature_2m                , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%spec_humidity_veg_2m          , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%spec_humidity_bare_2m         , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%spec_humidity_2m              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%spec_humidity_surface         , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%spec_humid_sfc_veg            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%spec_humid_sfc_bare           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%dewpoint_veg_2m               , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%dewpoint_bare_2m              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%diag%dewpoint_2m                   , end_of_day, daily_mean_count, daily_mean)

! Begin noahmp%state variables

  call accumulate(noahmp%state%temperature_soil_mp             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%temperature_snow             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%temperature_canopy_air       , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%temperature_radiative        , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%temperature_leaf             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%temperature_ground           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%temperature_bare_grd         , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%temperature_veg_grd          , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%vapor_pres_canopy_air        , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%soil_liquid_vol_mp              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%soil_moisture_vol_mp            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%snow_water_equiv             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%snow_level_ice               , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%snow_level_liquid            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%canopy_liquid                , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%canopy_ice                   , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%aquifer_water                , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%saturated_water              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%lake_water                   , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%soil_moisture_wtd            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%eq_soil_water_vol            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%leaf_carbon                  , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%root_carbon                  , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%stem_carbon                  , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%wood_carbon                  , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%soil_carbon_stable           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%soil_carbon_fast             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%grain_carbon                 , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%foliage_nitrogen             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%snow_water_equiv_old         , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%snow_depth                   , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%state%snow_age                     , end_of_day, daily_mean_count, daily_mean)

! Begin noahmp%flux variables

  call accumulate(noahmp%flux%sw_absorbed_total             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%sw_reflected_total            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%lw_absorbed_total             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%sensible_heat_total           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%transpiration_heat            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%latent_heat_canopy            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%latent_heat_ground            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%latent_heat_total             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%ground_heat_total             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%precip_adv_heat_total         , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%sw_absorbed_veg               , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%sw_absorbed_ground            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%lw_absorbed_grd_veg           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%lw_absorbed_leaf              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%lw_absorbed_grd_bare          , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%sensible_heat_grd_veg         , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%sensible_heat_leaf            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%sensible_heat_grd_bar         , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%latent_heat_trans             , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%latent_heat_leaf              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%latent_heat_grd_veg           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%latent_heat_grd_bare          , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%snow_sublimation              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%ground_heat_veg               , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%ground_heat_bare              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%precip_adv_heat_veg           , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%precip_adv_heat_grd_v         , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%precip_adv_heat_grd_b         , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%transpiration                 , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%evaporation_canopy            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%evaporation_soil              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%runoff_surface                , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%runoff_baseflow               , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%snowmelt_out                  , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%snowmelt_shallow              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%snowmelt_shallow_1            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%snowmelt_shallow_2            , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%deep_recharge                 , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%recharge                      , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%par_absorbed                  , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%photosynthesis                , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%net_eco_exchange              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%global_prim_prod              , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%net_prim_prod                 , end_of_day, daily_mean_count, daily_mean)
  call accumulate(noahmp%flux%canopy_heat_storage           , end_of_day, daily_mean_count, daily_mean)

  end subroutine DailyMeanNoahMP

  subroutine MonthlyMeanNoahMP(noahmp, end_of_month, monthly_mean_count)
  
  use ufsLandNoahMPType
  
  type(noahmp_type)    :: noahmp

  logical :: end_of_month
  integer :: monthly_mean_count

! Begin noahmp%static variables

  call accumulate(noahmp%static%vegetation_category         , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%static%soil_category               , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%static%slope_category              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%static%soil_interface_depth        , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%static%ice_flag                    , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%static%surface_type                , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%static%crop_type                   , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%static%temperature_soil_bot        , end_of_month, monthly_mean_count, monthly_mean)

! Begin noahmp%model variables

  call accumulate(noahmp%model%latitude                     , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%longitude                    , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%cosine_zenith                , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%forcing_height               , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%vegetation_fraction          , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%max_vegetation_frac          , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%active_snow_levels           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%interface_depth              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%snow_soil_thickness          , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%leaf_area_index              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%stem_area_index              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%growing_deg_days             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%plant_growth_stage           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%cm_noahmp                    , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%ch_noahmp                    , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%ch_vegetated                 , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%ch_bare_ground               , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%ch_leaf                      , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%ch_below_canopy              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%ch_vegetated_2m              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%ch_bare_ground_2m            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%friction_velocity            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%rs_sunlit                    , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%rs_shaded                    , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%leaf_air_resistance          , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%pbl_height                   , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%mo_length_inverse            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%heat_flux_multiplier         , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%model%moisture_flux_multiplier     , end_of_month, monthly_mean_count, monthly_mean)

! Begin noahmp%forcing variables

  call accumulate(noahmp%forcing%temperature_forcing        , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%forcing%specific_humidity_forcing  , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%forcing%surface_pressure_forcing   , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%forcing%wind_speed_forcing         , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%forcing%downward_longwave_forcing  , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%forcing%downward_shortwave_forcing , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%forcing%precipitation_forcing      , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%forcing%precip_convective          , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%forcing%precip_non_convective      , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%forcing%precip_snow                , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%forcing%precip_graupel             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%forcing%precip_hail                , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%forcing%snowfall                   , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%forcing%rainfall                   , end_of_month, monthly_mean_count, monthly_mean)

! Begin noahmp%diag variables

  call accumulate(noahmp%diag%z0_total                      , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%z0h_total                     , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%albedo_total                  , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%albedo_direct                 , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%albedo_diffuse                , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%albedo_direct_snow            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%albedo_diffuse_snow           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%emissivity_total              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%canopy_gap_fraction           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%incanopy_gap_fraction         , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%precip_frozen_frac            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%snow_cover_fraction           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%canopy_wet_fraction           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%canopy_water                  , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%depth_water_table             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%lai_sunlit                    , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%lai_shaded                    , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%snow_ice_frac_old             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%snow_albedo_old               , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%evaporation_potential         , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%soil_moisture_total           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%temperature_veg_2m            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%temperature_bare_2m           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%temperature_2m                , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%spec_humidity_veg_2m          , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%spec_humidity_bare_2m         , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%spec_humidity_2m              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%spec_humidity_surface         , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%spec_humid_sfc_veg            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%spec_humid_sfc_bare           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%dewpoint_veg_2m               , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%dewpoint_bare_2m              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%diag%dewpoint_2m                   , end_of_month, monthly_mean_count, monthly_mean)

! Begin noahmp%state variables

  call accumulate(noahmp%state%temperature_soil_mp             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%temperature_snow             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%temperature_canopy_air       , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%temperature_radiative        , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%temperature_leaf             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%temperature_ground           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%temperature_bare_grd         , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%temperature_veg_grd          , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%vapor_pres_canopy_air        , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%soil_liquid_vol_mp              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%soil_moisture_vol_mp            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%snow_water_equiv             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%snow_level_ice               , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%snow_level_liquid            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%canopy_liquid                , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%canopy_ice                   , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%aquifer_water                , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%saturated_water              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%lake_water                   , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%soil_moisture_wtd            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%eq_soil_water_vol            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%leaf_carbon                  , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%root_carbon                  , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%stem_carbon                  , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%wood_carbon                  , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%soil_carbon_stable           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%soil_carbon_fast             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%grain_carbon                 , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%foliage_nitrogen             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%snow_water_equiv_old         , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%snow_depth                   , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%state%snow_age                     , end_of_month, monthly_mean_count, monthly_mean)

! Begin noahmp%flux variables

  call accumulate(noahmp%flux%sw_absorbed_total             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%sw_reflected_total            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%lw_absorbed_total             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%sensible_heat_total           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%transpiration_heat            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%latent_heat_canopy            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%latent_heat_ground            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%latent_heat_total             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%ground_heat_total             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%precip_adv_heat_total         , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%sw_absorbed_veg               , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%sw_absorbed_ground            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%lw_absorbed_grd_veg           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%lw_absorbed_leaf              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%lw_absorbed_grd_bare          , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%sensible_heat_grd_veg         , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%sensible_heat_leaf            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%sensible_heat_grd_bar         , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%latent_heat_trans             , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%latent_heat_leaf              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%latent_heat_grd_veg           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%latent_heat_grd_bare          , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%snow_sublimation              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%ground_heat_veg               , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%ground_heat_bare              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%precip_adv_heat_veg           , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%precip_adv_heat_grd_v         , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%precip_adv_heat_grd_b         , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%transpiration                 , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%evaporation_canopy            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%evaporation_soil              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%runoff_surface                , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%runoff_baseflow               , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%snowmelt_out                  , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%snowmelt_shallow              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%snowmelt_shallow_1            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%snowmelt_shallow_2            , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%deep_recharge                 , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%recharge                      , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%par_absorbed                  , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%photosynthesis                , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%net_eco_exchange              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%global_prim_prod              , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%net_prim_prod                 , end_of_month, monthly_mean_count, monthly_mean)
  call accumulate(noahmp%flux%canopy_heat_storage           , end_of_month, monthly_mean_count, monthly_mean)

  end subroutine MonthlyMeanNoahMP

  subroutine DiurnalNoahMP(noahmp, end_of_month, diurnal_count, num_diurnal)
  
  use ufsLandNoahMPType
  
  type(noahmp_type)    :: noahmp

  logical :: end_of_month
  integer :: diurnal_count
  integer :: num_diurnal

! Begin noahmp%static variables

  call accumulate(noahmp%static%vegetation_category         , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%static%soil_category               , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%static%slope_category              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%static%soil_interface_depth        , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%static%ice_flag                    , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%static%surface_type                , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%static%crop_type                   , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%static%temperature_soil_bot        , end_of_month, diurnal_count, diurnal, num_diurnal)

! Begin noahmp%model variables

  call accumulate(noahmp%model%latitude                     , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%longitude                    , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%cosine_zenith                , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%forcing_height               , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%vegetation_fraction          , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%max_vegetation_frac          , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%active_snow_levels           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%interface_depth              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%snow_soil_thickness          , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%leaf_area_index              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%stem_area_index              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%growing_deg_days             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%plant_growth_stage           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%cm_noahmp                    , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%ch_noahmp                    , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%ch_vegetated                 , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%ch_bare_ground               , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%ch_leaf                      , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%ch_below_canopy              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%ch_vegetated_2m              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%ch_bare_ground_2m            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%friction_velocity            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%rs_sunlit                    , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%rs_shaded                    , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%leaf_air_resistance          , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%pbl_height                   , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%mo_length_inverse            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%heat_flux_multiplier         , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%model%moisture_flux_multiplier     , end_of_month, diurnal_count, diurnal, num_diurnal)

! Begin noahmp%forcing variables

  call accumulate(noahmp%forcing%temperature_forcing        , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%forcing%specific_humidity_forcing  , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%forcing%surface_pressure_forcing   , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%forcing%wind_speed_forcing         , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%forcing%downward_longwave_forcing  , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%forcing%downward_shortwave_forcing , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%forcing%precipitation_forcing      , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%forcing%precip_convective          , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%forcing%precip_non_convective      , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%forcing%precip_snow                , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%forcing%precip_graupel             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%forcing%precip_hail                , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%forcing%snowfall                   , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%forcing%rainfall                   , end_of_month, diurnal_count, diurnal, num_diurnal)

! Begin noahmp%diag variables

  call accumulate(noahmp%diag%z0_total                      , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%z0h_total                     , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%albedo_total                  , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%albedo_direct                 , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%albedo_diffuse                , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%albedo_direct_snow            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%albedo_diffuse_snow           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%emissivity_total              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%canopy_gap_fraction           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%incanopy_gap_fraction         , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%precip_frozen_frac            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%snow_cover_fraction           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%canopy_wet_fraction           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%canopy_water                  , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%depth_water_table             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%lai_sunlit                    , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%lai_shaded                    , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%snow_ice_frac_old             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%snow_albedo_old               , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%evaporation_potential         , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%soil_moisture_total           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%temperature_veg_2m            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%temperature_bare_2m           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%temperature_2m                , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%spec_humidity_veg_2m          , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%spec_humidity_bare_2m         , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%spec_humidity_2m              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%spec_humidity_surface         , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%spec_humid_sfc_veg            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%spec_humid_sfc_bare           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%dewpoint_veg_2m               , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%dewpoint_bare_2m              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%diag%dewpoint_2m                   , end_of_month, diurnal_count, diurnal, num_diurnal)

! Begin noahmp%state variables

  call accumulate(noahmp%state%temperature_soil_mp             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%temperature_snow             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%temperature_canopy_air       , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%temperature_radiative        , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%temperature_leaf             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%temperature_ground           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%temperature_bare_grd         , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%temperature_veg_grd          , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%vapor_pres_canopy_air        , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%soil_liquid_vol_mp              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%soil_moisture_vol_mp            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%snow_water_equiv             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%snow_level_ice               , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%snow_level_liquid            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%canopy_liquid                , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%canopy_ice                   , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%aquifer_water                , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%saturated_water              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%lake_water                   , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%soil_moisture_wtd            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%eq_soil_water_vol            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%leaf_carbon                  , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%root_carbon                  , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%stem_carbon                  , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%wood_carbon                  , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%soil_carbon_stable           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%soil_carbon_fast             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%grain_carbon                 , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%foliage_nitrogen             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%snow_water_equiv_old         , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%snow_depth                   , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%state%snow_age                     , end_of_month, diurnal_count, diurnal, num_diurnal)

! Begin noahmp%flux variables

  call accumulate(noahmp%flux%sw_absorbed_total             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%sw_reflected_total            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%lw_absorbed_total             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%sensible_heat_total           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%transpiration_heat            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%latent_heat_canopy            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%latent_heat_ground            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%latent_heat_total             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%ground_heat_total             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%precip_adv_heat_total         , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%sw_absorbed_veg               , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%sw_absorbed_ground            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%lw_absorbed_grd_veg           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%lw_absorbed_leaf              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%lw_absorbed_grd_bare          , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%sensible_heat_grd_veg         , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%sensible_heat_leaf            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%sensible_heat_grd_bar         , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%latent_heat_trans             , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%latent_heat_leaf              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%latent_heat_grd_veg           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%latent_heat_grd_bare          , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%snow_sublimation              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%ground_heat_veg               , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%ground_heat_bare              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%precip_adv_heat_veg           , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%precip_adv_heat_grd_v         , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%precip_adv_heat_grd_b         , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%transpiration                 , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%evaporation_canopy            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%evaporation_soil              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%runoff_surface                , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%runoff_baseflow               , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%snowmelt_out                  , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%snowmelt_shallow              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%snowmelt_shallow_1            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%snowmelt_shallow_2            , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%deep_recharge                 , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%recharge                      , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%par_absorbed                  , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%photosynthesis                , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%net_eco_exchange              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%global_prim_prod              , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%net_prim_prod                 , end_of_month, diurnal_count, diurnal, num_diurnal)
  call accumulate(noahmp%flux%canopy_heat_storage           , end_of_month, diurnal_count, diurnal, num_diurnal)

  end subroutine DiurnalNoahMP

end module ufsLandMeanOutput
