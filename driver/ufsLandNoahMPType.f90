module ufsLandNoahMPType

use NamelistRead
use ufsLandGenericType
use machine , only : kind_phys

implicit none
save
private

type :: noahmp_static_type

  real(kind=kind_phys) :: timestep                ! time interval [s]
  integer              :: vector_length           ! number of simulation grids
  integer              :: soil_source             ! soil type data source zobler or statsgo
  integer              :: veg_source              ! veg type data source umd or igbp
  integer              :: soil_levels             ! vertical soil layer dimension
  integer              :: max_snow_levels         ! maximum number of snow levels
  type(int1d)          :: vegetation_category     ! vegetation type (integer index)
  type(int1d)          :: soil_category           ! soil texture type (integer index)
  type(int1d)          :: slope_category          ! slope category (integer index)
  type(real1d)         :: soil_interface_depth    ! soil layer-bottom depth from surface [m]
  type(int1d)          :: ice_flag                ! ice flag (1->ice)
  type(int1d)          :: surface_type            ! surface type flag 1->soil; 2->lake
  type(int1d)          :: crop_type               ! crop type category
  type(real1d)         :: temperature_soil_bot    ! soil bottom boundary condition temperature [K]

end type noahmp_static_type

type :: noahmp_options_type

  integer :: dynamic_vegetation         ! option for dynamic vegetation
  integer :: canopy_stomatal_resistance ! option for canopy stomatal resistance
  integer :: soil_wetness               ! option for soil moisture factor for stomatal resistance
  integer :: runoff                     ! option for runoff and groundwater
  integer :: surface_exchange           ! option for surface layer drag coeff (ch & cm)
  integer :: supercooled_soilwater      ! option for supercooled liquid water (or ice fraction)
  integer :: frozen_soil_adjust         ! option for frozen soil permeability
  integer :: radiative_transfer         ! option for radiation transfer
  integer :: snow_albedo                ! option for ground snow surface albedo
  integer :: precip_partition           ! option for partitioning  precipitation into rainfall & snowfall
  integer :: soil_temp_lower_bdy        ! option for lower boundary condition of soil temperature
  integer :: soil_temp_time_scheme      ! option for snow/soil temperature time scheme (only layer 1)
  integer :: thermal_roughness_scheme   ! option for thermal roughness length
  integer :: surface_evap_resistance    ! option for surface resistent to evaporation/sublimation
  integer :: glacier                    ! option for glacier treatment

end type noahmp_options_type

type :: noahmp_model_type

  integer               :: i_location               ! grid index
  integer               :: j_location               ! grid index (not used in ccpp)
  integer               :: year_length              ! number of days in the current year
  real(kind=kind_phys)  :: julian_day               ! julian day of year [floating point]
  type(real1d)          :: latitude                 ! latitude [radians]
  type(real1d)          :: cosine_zenith            ! cosine solar zenith angle [-1,1]
  type(real1d)          :: forcing_height           ! forcing height [m]
  type(real1d)          :: vegetation_fraction      ! vegetation fraction [0.0-1.0]
  type(real1d)          :: max_vegetation_frac      ! annual maximum vegetation fraction [0.0-1.0]
  type(real1d)          :: snow_levels              ! active snow levels [-]
  type(real2d)          :: interface_depth          ! layer-bottom depth from snow surf [m]
  type(real2d)          :: snow_soil_thickness      ! thickness of each snow/soil level [m]
  type(real1d)          :: leaf_area_index          ! leaf area index [-]
  type(real1d)          :: stem_area_index          ! stem area index [-]
  type(real1d)          :: growing_deg_days         ! growing degree days [-]
  type(int1d)           :: plant_growth_stage       ! plant growing stage [-]
  type(real1d)          :: cm_noahmp                ! grid momentum drag coefficient [m/s]
  type(real1d)          :: ch_noahmp                ! grid heat exchange coefficient [m/s]
  type(real1d)          :: ch_vegetated             ! vegetated heat exchange coefficient [m/s]
  type(real1d)          :: ch_bare_ground           ! bare-ground heat exchange coefficient [m/s]
  type(real1d)          :: ch_leaf                  ! leaf exchange coefficient [m/s]
  type(real1d)          :: ch_below_canopy          ! below-canopy exchange coefficient [m/s]
  type(real1d)          :: ch_vegetated_2m          ! 2-m vegetated  heat exchange coefficient [m/s]
  type(real1d)          :: ch_bare_ground_2m        ! 2-m bare-ground heat exchange coefficient [m/s]
  type(real1d)          :: friction_velocity        ! friction velocity [m/s]
  type(real1d)          :: rs_sunlit                ! sunlit leaf stomatal resistance [s/m]
  type(real1d)          :: rs_shaded                ! shaded leaf stomatal resistance [s/m]
  type(real1d)          :: leaf_air_resistance      ! leaf boundary layer resistance [s/m]
  type(real1d)          :: pbl_height               ! height of pbl [m]
  type(real1d)          :: mo_length_inverse        ! reciprocle of M-O length [1/m]
  type(real1d)          :: heat_flux_multiplier     ! heat flux multiplier [W/m^2/K]
  type(real1d)          :: moisture_flux_multiplier ! moisture flux multiplier [kg/m^2/s]

end type noahmp_model_type

type :: noahmp_forcing_type

  type(real1d)  :: temperature_forcing        ! temperature at forcing level [K]
  type(real1d)  :: specific_humidity_forcing  ! specific humidity at forcing level [mm/s]
  type(real1d)  :: surface_pressure_forcing   ! surface pressure [Pa]
  type(real1d)  :: wind_speed_forcing         ! wind speed at forcing level [m/s]
  type(real1d)  :: downward_longwave_forcing  ! surface downward longwave forcing [W/m2]
  type(real1d)  :: downward_shortwave_forcing ! surface downward shortwave forcing [W/m2]
  type(real1d)  :: precipitation_forcing      ! precipitation forcing [mm/s]
  type(real1d)  :: precip_convective          ! convective precipitation [mm/s]
  type(real1d)  :: precip_non_convective      ! non-convective precipitation [mm/s]
  type(real1d)  :: precip_snow                ! snow precipitation [mm/s]
  type(real1d)  :: precip_graupel             ! graupel precipitation [mm/s]
  type(real1d)  :: precip_hail                ! hail precipitation [mm/s]
  type(real1d)  :: snowfall                   ! land model partitioned snowfall [mm/s]
  type(real1d)  :: rainfall                   ! land model partitioned rainfall [mm/s]

end type noahmp_forcing_type

type :: noahmp_diag_type

  type(real1d)  :: z0_total               ! weighted z0 sent to coupled model [m]
  type(real1d)  :: z0h_total              ! weighted z0h sent to coupled model [m]
  type(real1d)  :: albedo_total           ! total surface albedo [-]
  type(real2d)  :: albedo_direct          ! direct vis/nir albedo [-]
  type(real2d)  :: albedo_diffuse         ! diffuse vis/nir albedo [-]
  type(real2d)  :: albedo_direct_snow     ! direct vis/nir snow albedo [-]
  type(real2d)  :: albedo_diffuse_snow    ! diffuse vis/nir snow albedo [-]
  type(real1d)  :: emissivity_total       ! grid emissivity [-]
  type(real1d)  :: canopy_gap_fraction    ! between canopy gap fraction [-]
  type(real1d)  :: incanopy_gap_fraction  ! within canopy gap fraction for beam [-]
  type(real1d)  :: precip_frozen_frac     ! precipitation snow fraction [-]
  type(real1d)  :: snow_cover_fraction    ! snow cover fraction on the ground [-]
  type(real1d)  :: canopy_wet_fraction    ! wetted or snowed fraction of canopy [-]
  type(real1d)  :: canopy_water           ! canopy-intercepted water [mm]
  type(real1d)  :: depth_water_table      ! depth to water table [m]
  type(real1d)  :: lai_sunlit             ! sunlit leaf area index [m2/m2]
  type(real1d)  :: lai_shaded             ! shaded leaf area index [m2/m2]
  type(real2d)  :: snow_ice_frac_old      ! snow ice fraction at last timestep [-]
  type(real1d)  :: snow_albedo_old        ! snow albedo at last time step [-]
  type(real1d)  :: evaporation_potential  ! potential evaporation [mm/s]
  type(real1d)  :: soil_moisture_total    ! total soil moisture in all levels [mm]
  type(real1d)  :: temperature_veg_2m     ! vegetated 2-m air temperature [K]
  type(real1d)  :: temperature_bare_2m    ! bare ground 2-m air temperature [K]
  type(real1d)  :: temperature_2m         ! composite 2-m air temperature [K]
  type(real1d)  :: spec_humidity_veg_2m   ! vegetated 2-m air specific humidity [K]
  type(real1d)  :: spec_humidity_bare_2m  ! bare ground 2-m air specfic humidity [K]
  type(real1d)  :: spec_humidity_2m       ! composite 2-m air specfic humidity [K]
  type(real1d)  :: spec_humidity_surface  ! surface specific humidty [kg/kg]

end type noahmp_diag_type

type :: noahmp_state_type

  type(real2d)  :: temperature_soil        ! soil temperature [K]
  type(real2d)  :: temperature_snow        ! snow temperature [K]
  type(real1d)  :: temperature_canopy_air  ! canopy air temperature [K]
  type(real1d)  :: temperature_radiative   ! surface radiative temperature [K]
  type(real1d)  :: temperature_leaf        ! leaf temperature [K]
  type(real1d)  :: temperature_ground      ! grid ground surface temperature [K]
  type(real1d)  :: temperature_bare_grd    ! bare ground surface temperature [K]
  type(real1d)  :: temperature_veg_grd     ! below_canopy ground surface temperature [K]
  type(real1d)  :: vapor_pres_canopy_air   ! canopy air vapor pressure [Pa]
  type(real2d)  :: soil_liquid_vol         ! volumetric liquid soil moisture [m3/m3]
  type(real2d)  :: soil_moisture_vol       ! volumetric soil moisture (ice + liq.) [m3/m3]
  type(real1d)  :: snow_water_equiv        ! snow water equivalent [kg/m2]
  type(real2d)  :: snow_level_ice          ! snow level ice [mm]
  type(real2d)  :: snow_level_liquid       ! snow level liquid [mm]
  type(real1d)  :: canopy_liquid           ! canopy-intercepted liquid [mm]
  type(real1d)  :: canopy_ice              ! canopy-intercepted ice [mm]
  type(real1d)  :: aquifer_water           ! water storage in aquifer [mm]
  type(real1d)  :: saturated_water         ! water in aquifer+saturated soil [mm]
  type(real1d)  :: lake_water              ! lake water storage [mm]
  type(real1d)  :: soil_moisture_wtd       ! (opt_run=5) soil water content between bottom of the soil and water table [m3/m3]
  type(real2d)  :: eq_soil_water_vol       ! (opt_run=5) equilibrium soil water content [m3/m3]
  type(real1d)  :: leaf_carbon             ! leaf mass [g/m2]
  type(real1d)  :: root_carbon             ! mass of fine roots [g/m2]
  type(real1d)  :: stem_carbon             ! stem mass [g/m2]
  type(real1d)  :: wood_carbon             ! mass of wood (incl. woody roots) [g/m2]
  type(real1d)  :: soil_carbon_stable      ! stable soil carbon [g/m2]
  type(real1d)  :: soil_carbon_fast        ! short-lived soil carbon [g/m2]
  type(real1d)  :: grain_carbon            ! grain mass [g/m2]
  type(real1d)  :: foliage_nitrogen        ! foliage nitrogen [%] [1-saturated]
  type(real1d)  :: snow_water_equiv_old    ! snow water equivalent at last time step [kg/m2]
  type(real1d)  :: snow_depth              ! snow depth [m]
  type(real1d)  :: snow_age                ! non-dimensional snow age [-]

end type noahmp_state_type

type :: noahmp_flux_type

  type(real1d)  :: sw_absorbed_total       ! total absorbed solar radiation [W/m2]
  type(real1d)  :: sw_reflected_total      ! total reflected solar radiation [W/m2]
  type(real1d)  :: lw_absorbed_total       ! total net lw rad [W/m2]  [+ to atm]
  type(real1d)  :: sensible_heat_total     ! total sensible heat [W/m2] [+ to atm]
  type(real1d)  :: transpiration_heat      ! transpiration heat flux [W/m2] [+ to atm]
  type(real1d)  :: latent_heat_canopy      ! canopy evaporation heat flux [W/m2] [+ to atm]
  type(real1d)  :: latent_heat_ground      ! ground evaporation heat flux [W/m2] [+ to atm]
  type(real1d)  :: latent_heat_total       ! total heat flux [W/m2] [+ to atm]
  type(real1d)  :: ground_heat_total       ! ground heat flux [W/m2]   [+ to soil]
  type(real1d)  :: precip_adv_heat_total   ! precipitation advected heat - total [W/m2)
  type(real1d)  :: sw_absorbed_veg         ! solar radiation absorbed by vegetation [W/m2]
  type(real1d)  :: sw_absorbed_ground      ! solar radiation absorbed by ground [W/m2]
  type(real1d)  :: lw_absorbed_grd_veg     ! below-canopy ground absorbed longwave radiation [W/m2]
  type(real1d)  :: lw_absorbed_leaf        ! leaf absorbed longwave radiation [W/m2]
  type(real1d)  :: lw_absorbed_grd_bare    ! bare ground net longwave radiation [W/m2]
  type(real1d)  :: sensible_heat_grd_veg   ! below-canopy ground sensible heat flux [W/m2]
  type(real1d)  :: sensible_heat_leaf      ! leaf-to-canopy sensible heat flux [W/m2]
  type(real1d)  :: sensible_heat_grd_bar   ! bare ground sensible heat flux [W/m2]
  type(real1d)  :: latent_heat_trans       ! transpiration [W/m2]
  type(real1d)  :: latent_heat_leaf        ! leaf evaporation [W/m2]
  type(real1d)  :: latent_heat_grd_veg     ! below-canopy ground evaporation heat flux [W/m2]
  type(real1d)  :: latent_heat_grd_bare    ! bare ground evaporation heat flux [W/m2]
  type(real1d)  :: snow_sublimation        ! snow sublimation [W/m2]
  type(real1d)  :: ground_heat_veg         ! below-canopy ground heat flux [W/m2]
  type(real1d)  :: ground_heat_bare        ! bare ground heat flux [W/m2]
  type(real1d)  :: precip_adv_heat_veg     ! precipitation advected heat - vegetation net [W/m2]
  type(real1d)  :: precip_adv_heat_grd_v   ! precipitation advected heat - below-canopy net [W/m2]
  type(real1d)  :: precip_adv_heat_grd_b   ! precipitation advected heat - bare ground net [W/m2]
  type(real1d)  :: transpiration           ! transpiration [mm/s]
  type(real1d)  :: evaporation_canopy      ! canopy evaporation [mm/s]
  type(real1d)  :: evaporation_soil        ! soil surface evaporation [mm/s]
  type(real1d)  :: runoff_surface          ! surface runoff [mm/s] 
  type(real1d)  :: runoff_baseflow         ! baseflow runoff [mm/s]
  type(real1d)  :: snowmelt_out            ! snowmelt out bottom of pack [mm/s]
  type(real1d)  :: snowmelt_shallow        ! shallow snow melt [mm/timestep]
  type(real1d)  :: snowmelt_shallow_1      ! additional shallow snow melt [mm/timestep]
  type(real1d)  :: snowmelt_shallow_2      ! additional shallow snow melt [mm/timestep]
  type(real1d)  :: deep_recharge           ! (opt_run=5) recharge to or from the water table when deep [m]
  type(real1d)  :: recharge                ! (opt_run=5) recharge to or from the water table when shallow [m]
  type(real1d)  :: par_absorbed            ! absorbed photosynthesis active radiation [W/m2]
  type(real1d)  :: photosynthesis          ! total photosynthesis [umol CO2/m2/s] [+ out]
  type(real1d)  :: net_eco_exchange        ! net ecosystem exchange [g/m2/s CO2]
  type(real1d)  :: global_prim_prod        ! global primary production [g/m2/s C]
  type(real1d)  :: net_prim_prod           ! net primary productivity [g/m2/s C]

end type noahmp_flux_type

type, public :: noahmp_type

  type (noahmp_static_type)      :: static
  type (noahmp_options_type)     :: options
  type (noahmp_model_type)       :: model
  type (noahmp_forcing_type)     :: forcing
  type (noahmp_diag_type)        :: diag
  type (noahmp_state_type)       :: state
  type (noahmp_flux_type)        :: flux

  contains

    procedure, public  :: Init
    procedure, private :: InitRestart
    procedure, public  :: TransferNamelist         
    procedure, public  :: InitStates         

end type noahmp_type

contains   

  subroutine Init(this, namelist, vector_length)

    class(noahmp_type) :: this
    type(namelist_type) :: namelist
    integer :: vector_length, soil_levels, snow_index

    this%static%vector_length   = vector_length
    this%static%soil_levels     = namelist%num_soil_levels
    this%static%max_snow_levels = namelist%num_snow_levels
    soil_levels = namelist%num_soil_levels
    snow_index = -1*namelist%num_snow_levels + 1
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Begin noahmp%static variables

    call InitInt1d (this%static%vegetation_category , &
                    vector_length                   , &
                    "vegetation_category"           , &
                    "categorical vegetation type"   , &
                    ""                              , &
                    namelist%output_names, namelist%restart_names)

    call InitInt1d (this%static%soil_category , &
                    vector_length             , &
                    "soil_category"           , &
                    "categorical soil type"   , &
                    ""                        , &
                    namelist%output_names, namelist%restart_names)

    call InitInt1d (this%static%slope_category , &
                    vector_length              , &
                    "slope_category"           , &
                    "categorical slope type"   , &
                    ""                         , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%static%soil_interface_depth , &
                    vector_length                    , &
                    "soil_interface_depth"           , &
                    "depths of soil interface"       , &
                    "m"                              , &
                    namelist%output_names, namelist%restart_names)

    call InitInt1d (this%static%ice_flag , &
                    vector_length        , &
                    "ice_flag"           , &
                    "ice flag: 1->ice"   , &
                    ""                   , &
                    namelist%output_names, namelist%restart_names)

    call InitInt1d (this%static%surface_type              , &
                    vector_length                         , &
                    "surface_type"                        , &
                    "surface type flag: 1->soil; 2->lake" , &
                    ""                                    , &
                    namelist%output_names, namelist%restart_names)

    call InitInt1d (this%static%crop_type   , &
                    vector_length           , &
                    "crop_type"             , &
                    "categorical crop type" , &
                    ""                      , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%static%temperature_soil_bot           , &
                    vector_length                              , &
                    "temperature_soil_bot"                     , &
                    "deep soil temperature boundary condition" , &
                    "K"                                        , &
                    namelist%output_names, namelist%restart_names)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Begin noahmp%model variables

    call InitReal1d(this%model%latitude  , &
                    vector_length        , &
                    "latitude"           , &
                    "grid cell latitude" , &
                    "degrees_north"      , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%cosine_zenith       , &
                    vector_length                  , &
                    "cosine_zenith"                , &
                    "cosine of solar zenith angle" , &
                    "-"                            , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%forcing_height , &
                    vector_length             , &
                    "forcing_height"          , &
                    "height of forcing"       , &
                    "m"                       , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%vegetation_fraction      , &
                    vector_length                       , &
                    "vegetation_fraction"               , &
                    "vegetation areal coverage on grid" , &
                    "fraction"                          , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%max_vegetation_frac              , &
                    vector_length                               , &
                    "max_vegetation_frac"                       , &
                    "maximum vegetation areal coverage on grid" , &
                    "fraction"                                  , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%snow_levels , &
                    vector_length          , &
                    "snow_levels"          , &
                    "active snow levels"   , &
                    "-"                    , &
                    namelist%output_names, namelist%restart_names)

    call InitReal2d(this%model%interface_depth                , &
                    vector_length                             , &
                    snow_index, soil_levels                   , &
                    "interface_depth"                         , &
                    "depth to layer bottom from snow surface" , &
                    "m"                                       , &
                    namelist%output_names, namelist%restart_names)

    call InitReal2d(this%model%snow_soil_thickness , &
                    vector_length                  , &
                    snow_index, soil_levels        , &
                    "snow_soil_thickness"          , &
                    "thickness of snow/soil level" , &
                    "m"                            , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%leaf_area_index , &
                    vector_length              , &
                    "leaf_area_index"          , &
                    "leaf area per grid area"  , &
                    "-"                        , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%stem_area_index , &
                    vector_length              , &
                    "stem_area_index"          , &
                    "stem area per grid area"  , &
                    "-"                        , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%growing_deg_days       , &
                    vector_length                     , &
                    "growing_deg_days"                , &
                    "accumulated growing degree days" , &
                    "K"                               , &
                    namelist%output_names, namelist%restart_names)

    call InitInt1d (this%model%plant_growth_stage , &
                    vector_length                 , &
                    "plant_growth_stage"          , &
                    "plant growth stage"          , &
                    "-"                           , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%cm_noahmp                  , &
                    vector_length                         , &
                    "cm_noahmp"                           , &
                    "surface exchange coeff for momentum" , &
                    "m/s"                                 , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%ch_noahmp                     , &
                    vector_length                            , &
                    "ch_noahmp"                              , &
                    "surface exchange coeff heat & moisture" , &
                    "m/s"                                    , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%ch_vegetated                                          , &
                    vector_length                                                    , &
                    "ch_vegetated"                                                   , &
                    "surface exchange coeff heat & moisture over vegetated fraction" , &
                    "m/s"                                                            , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%ch_bare_ground                                   , &
                    vector_length                                               , &
                    "ch_bare_ground"                                            , &
                    "surface exchange coeff heat & moisture over bare fraction" , &
                    "m/s"                                                       , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%ch_leaf                                       , &
                    vector_length                                            , &
                    "ch_leaf"                                                , &
                    "surface exchange coeff heat & moisture at leaf surface" , &
                    "m/s"                                                    , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%ch_below_canopy                            , &
                    vector_length                                         , &
                    "ch_below_canopy"                                     , &
                    "surface exchange coeff heat & moisture below canopy" , &
                    "m/s"                                                 , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%ch_vegetated_2m                                          , &
                    vector_length                                                       , &
                    "ch_vegetated_2m"                                                   , &
                    "2m surface exchange coeff heat & moisture over vegetated fraction" , &
                    "m/s"                                                               , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%ch_bare_ground_2m                                   , &
                    vector_length                                                  , &
                    "ch_bare_ground_2m"                                            , &
                    "2m surface exchange coeff heat & moisture over bare fraction" , &
                    "m/s"                                                          , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%friction_velocity , &
                    vector_length                , &
                    "friction_velocity"          , &
                    "friction velocity"          , &
                    "m/s"                        , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%rs_sunlit              , &
                    vector_length                     , &
                    "rs_sunlit"                       , &
                    "sunlit leaf stomatal resistance" , &
                    "s/m"                             , &
                    namelist%output_names, namelist%restart_names)
 
    call InitReal1d(this%model%rs_shaded              , &
                    vector_length                     , &
                    "rs_shaded"                       , &
                    "shaded leaf stomatal resistance" , &
                    "s/m"                             , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%leaf_air_resistance   , &
                    vector_length                    , &
                    "leaf_air_resistance"            , &
                    "leaf boundary layer resistance" , &
                    "s/m"                            , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%pbl_height                , &
                    vector_length                        , &
                    "pbl_height"                         , &
                    "height of planetary boundary layer" , &
                    "m"                                  , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%mo_length_inverse , &
                    vector_length                , &
                    "mo_length_inverse"          , &
                    "reciprocal of M-O length"   , &
                    "1/m"                        , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%heat_flux_multiplier , &
                    vector_length                   , &
                    "heat_flux_multiplier"          , &
                    "heat flux multiplier"          , &
                    "W/m^2/K"                       , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%model%moisture_flux_multiplier , &
                    vector_length                       , &
                    "moisture_flux_multiplier"          , &
                    "moisture flux multiplier"          , &
                    "kg/m^2/s"                          , &
                    namelist%output_names, namelist%restart_names)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Begin noahmp%forcing variables

    call InitReal1d(this%forcing%temperature_forcing        , &
                    vector_length                           , &
                    "temperature_forcing"                   , &
                    "temperature at forcing level"          , &
                    "K"                                     , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%forcing%specific_humidity_forcing  , &
                    vector_length                           , &
                    "specific_humidity_forcing"             , &
                    "specific humidtiy at forcing level"    , &
                    "kg/kg"                                 , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%forcing%surface_pressure_forcing   , &
                    vector_length                           , &
                    "surface_pressure_forcing"              , &
                    "atmospheric pressure at surface"       , &
                    "Pa"                                    , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%forcing%wind_speed_forcing         , &
                    vector_length                           , &
                    "wind_speed_forcing"                    , &
                    "wind speed at forcing level"           , &
                    "m/s"                                   , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%forcing%downward_longwave_forcing  , &
                    vector_length                           , &
                    "downward_longwave_forcing"             , &
                    "surface downward longwave forcing"     , &
                    "W/m2"                                  , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%forcing%downward_shortwave_forcing , &
                    vector_length                           , &
                    "downward_shortwave_forcing"            , &
                    "surface downward shortwave forcing"    , &
                    "W/m2"                                  , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%forcing%precipitation_forcing      , &
                    vector_length                           , &
                    "precipitation_forcing"                 , &
                    "precipitation forcing"                 , &
                    "mm/s"                                  , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%forcing%precip_convective          , &
                    vector_length                           , &
                    "precip_convective"                     , &
                    "convective component of precipitation" , &
                    "mm/s"                                  , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%forcing%precip_non_convective          , &
                    vector_length                               , &
                    "precip_non_convective"                     , &
                    "non-convective component of precipitation" , &
                    "mm/s"                                      , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%forcing%precip_snow , &
                    vector_length            , &
                    "precip_snow"            , &
                    "snow precipitation"     , &
                    "mm/s"                   , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%forcing%precip_graupel , &
                    vector_length               , &
                    "precip_graupel"            , &
                    "graupel precipitation"     , &
                    "mm/s"                      , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%forcing%precip_hail , &
                    vector_length            , &
                    "precip_hail"            , &
                    "hail precipitation"     , &
                    "mm/s"                   , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%forcing%snowfall             , &
                    vector_length                     , &
                    "snowfall"                        , &
                    "land model partitioned snowfall" , &
                    "mm/s"                            , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%forcing%rainfall             , &
                    vector_length                     , &
                    "rainfall"                        , &
                    "land model partitioned rainfall" , &
                    "mm/s"                            , &
                    namelist%output_names, namelist%restart_names)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Begin noahmp%diag variables

    call InitReal1d(this%diag%z0_total                                    , &
                    vector_length                                         , &
                    "z0_total"                                            , &
                    "grid composite surface roughness sent to atmosphere" , &
                    "m"                                                   , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%z0h_total                                   , &
                    vector_length                                         , &
                    "z0h_total"                                           , &
                    "grid composite surface thermal roughness sent to atmosphere" , &
                    "m"                                                   , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%albedo_total  , &
                    vector_length           , &
                    "albedo_total"          , &
                    "grid composite albedo" , &
                    "fraction"              , &
                    namelist%output_names, namelist%restart_names)

    call InitReal2d(this%diag%albedo_direct                 , &
                    vector_length                           , &
                    1, 2                                    , &
                    "albedo_direct"                         , &
                    "surface albedo - direct vis(1)/nir(2)" , &
                    "fraction"                              , &
                    namelist%output_names, namelist%restart_names)

    call InitReal2d(this%diag%albedo_diffuse                 , &
                    vector_length                            , &
                    1, 2                                     , &
                    "albedo_diffuse"                         , &
                    "surface albedo - diffuse vis(1)/nir(2)" , &
                    "fraction"                               , &
                    namelist%output_names, namelist%restart_names)

    call InitReal2d(this%diag%albedo_direct_snow         , &
                    vector_length                        , &
                    1, 2                                 , &
                    "albedo_direct_snow"                 , &
                    "snow albedo - direct vis(1)/nir(2)" , &
                    "fraction"                           , &
                    namelist%output_names, namelist%restart_names)

    call InitReal2d(this%diag%albedo_diffuse_snow         , &
                    vector_length                         , &
                    1, 2                                  , &
                    "albedo_diffuse_snow"                 , &
                    "snow albedo - diffuse vis(1)/nir(2)" , &
                    "fraction"                            , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%emissivity_total          , &
                    vector_length                       , &
                    "emissivity_total"                  , &
                    "grid composite surface emissivity" , &
                    "-"                                 , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%canopy_gap_fraction , &
                    vector_length                 , &
                    "canopy_gap_fraction"         , &
                    "between-canopy gap fraction" , &
                    "fraction"                    , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%incanopy_gap_fraction , &
                    vector_length                   , &
                    "incanopy_gap_fraction"         , &
                    "within-canopy gap fraction"    , &
                    "fraction"                      , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%precip_frozen_frac    , &
                    vector_length                   , &
                    "precip_frozen_frac"            , &
                    "precipitation frozen fraction" , &
                    "fraction"                      , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%snow_cover_fraction , &
                    vector_length                 , &
                    "snow_cover_fraction"         , &
                    "snow cover fraction"         , &
                    "fraction"                    , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%canopy_wet_fraction         , &
                    vector_length                         , &
                    "canopy_wet_fraction"                 , &
                    "wetted or snowed fraction of canopy" , &
                    "fraction"                            , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%canopy_water    , &
                    vector_length             , &
                    "canopy_water"            , &
                    "canopy moisture content" , &
                    "m"                       , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%depth_water_table , &
                    vector_length               , &
                    "depth_water_table"         , &
                    "depth to water table"      , &
                    "m"                         , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%lai_sunlit     , &
                    vector_length            , &
                    "lai_sunlit"             , &
                    "sunlit leaf area index" , &
                    "m2/m2"                  , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%lai_shaded     , &
                    vector_length            , &
                    "lai_shaded"             , &
                    "shaded leaf area index" , &
                    "m2/m2"                  , &
                    namelist%output_names, namelist%restart_names)

    call InitReal2d(this%diag%snow_ice_frac_old          , &
                    vector_length                        , &
                    snow_index, 0                        , &
                    "snow_ice_frac_old"                  , &
                    "snow ice fraction at last timestep" , &
                    "fraction"                           , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%snow_albedo_old       , &
                    vector_length                   , &
                    "snow_albedo_old"               , &
                    "snow albedo at last time step" , &
                    "fraction"                      , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%evaporation_potential , &
                    vector_length                   , &
                    "evaporation_potential"         , &
                    "potential evaporation"         , &
                    "mm/s"                          , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%soil_moisture_total       , &
                    vector_length                       , &
                    "soil_moisture_total"               , &
                    "total soil moisture in all levels" , &
                    "mm"                                , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%temperature_veg_2m                   , &
                    vector_length                                  , &
                    "temperature_veg_2m"                           , &
                    "2-m air temperature over vegetation fraction" , &
                    "K"                                            , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%temperature_bare_2m                   , &
                    vector_length                                   , &
                    "temperature_bare_2m"                           , &
                    "2-m air temperature over bare ground fraction" , &
                    "K"                                             , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%temperature_2m        , &
                    vector_length                   , &
                    "temperature_2m"                , &
                    "composite 2-m air temperature" , &
                    "K"                             , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%spec_humidity_veg_2m                       , &
                    vector_length                                        , &
                    "spec_humidity_veg_2m"                               , &
                    "2-m air specific humidity over vegetation fraction" , &
                    "kg/kg"                                              , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%spec_humidity_bare_2m                      , &
                    vector_length                                        , &
                    "spec_humidity_bare_2m"                              , &
                    "2-m air specific humidity over vegetation fraction" , &
                    "kg/kg"                                              , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%spec_humidity_2m           , &
                    vector_length                        , &
                    "spec_humidity_2m"                   , &
                    "composite 2-m air specfic humidity" , &
                    "kg/kg"                              , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%diag%spec_humidity_surface       , &
                    vector_length                         , &
                    "spec_humidity_surface"               , &
                    "kg/kg"                               , &
                    "diagnostic specific humidity at sfc" , &
                    namelist%output_names, namelist%restart_names)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Begin noahmp%state variables

    call InitReal2d(this%state%temperature_soil , &
                    vector_length               , &
                    1, soil_levels              , &
                    "temperature_soil"          , &
                    "soil level temperature"    , &
                    "K"                         , &
                    namelist%output_names, namelist%restart_names)

    call InitReal2d(this%state%temperature_snow , &
                    vector_length               , &
                    snow_index, 0               , &
                    "temperature_snow"          , &
                    "snow level temperature"    , &
                    "K"                         , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%temperature_canopy_air , &
                    vector_length                     , &
                    "temperature_canopy_air"          , &
                    "canopy air space temperature"    , &
                    "K"                               , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%temperature_radiative          , &
                    vector_length                             , &
                    "temperature_radiative"                   , &
                    "composite surface radiative temperature" , &
                    "K"                                       , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%temperature_leaf , &
                    vector_length               , &
                    "temperature_leaf"          , &
                    "leaf surface temperature"  , &
                    "K"                         , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%temperature_ground          , &
                    vector_length                          , &
                    "temperature_ground"                   , &
                    "composite ground surface temperature" , &
                    "K"                                    , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%temperature_bare_grd   , &
                    vector_length                     , &
                    "temperature_bare_grd"            , &
                    "bare ground surface temperature" , &
                    "K"                               , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%temperature_veg_grd            , &
                    vector_length                             , &
                    "temperature_veg_grd"                     , &
                    "below-canopy ground surface temperature" , &
                    "K"                                       , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%vapor_pres_canopy_air , &
                    vector_length                    , &
                    "vapor_pres_canopy_air"          , &
                    "canopy air vapor pressure"      , &
                    "Pa"                             , &
                    namelist%output_names, namelist%restart_names)

    call InitReal2d(this%state%soil_liquid_vol                , &
                    vector_length                             , &
                    1, soil_levels                            , &
                    "soil_liquid_vol"                         , &
                    "volumetric liquid content in soil level" , &
                    "m3/m3"                                   , &
                    namelist%output_names, namelist%restart_names)

    call InitReal2d(this%state%soil_moisture_vol                , &
                    vector_length                               , &
                    1, soil_levels                              , &
                    "soil_moisture_vol"                         , &
                    "volumetric moisture content in soil level" , &
                    "m3/m3"                                     , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%snow_water_equiv , &
                    vector_length               , &
                    "snow_water_equiv"          , &
                    "snow water equivalent"     , &
                    "mm"                        , &
                    namelist%output_names, namelist%restart_names)

    call InitReal2d(this%state%snow_level_ice , &
                    vector_length             , &
                    snow_index, 0             , &
                    "snow_level_ice"          , &
                    "snow level ice content"  , &
                    "mm"                      , &
                    namelist%output_names, namelist%restart_names)

    call InitReal2d(this%state%snow_level_liquid , &
                    vector_length                , &
                    snow_index, 0                , &
                    "snow_level_liquid"          , &
                    "snow level liquid content"  , &
                    "mm"                         , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%canopy_liquid    , &
                    vector_length               , &
                    "canopy_liquid"             , &
                    "canopy-intercepted liquid" , &
                    "mm"                        , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%canopy_ice    , &
                    vector_length            , &
                    "canopy_ice"             , &
                    "canopy-intercepted ice" , &
                    "mm"                     , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%aquifer_water   , &
                    vector_length              , &
                    "aquifer_water"            , &
                    "water storage in aquifer" , &
                    "mm"                       , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%saturated_water        , &
                    vector_length                     , &
                    "saturated_water"                 , &
                    "water in aquifer+saturated soil" , &
                    "mm"                              , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%lake_water , &
                    vector_length         , &
                    "lake_water"          , &
                    "lake water storage"  , &
                    "mm"                  , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%soil_moisture_wtd                                    , &
                    vector_length                                                   , &
                    "soil_moisture_wtd"                                             , &
                    "soil water content between bottom of the soil and water table" , &
                    "m3/m3"                                                         , &
                    namelist%output_names, namelist%restart_names)

    call InitReal2d(this%state%eq_soil_water_vol     , &
                    vector_length                    , &
                    1, soil_levels                   , &
                    "eq_soil_water_vol"              , &
                    "equilibrium soil water content" , &
                    "m3/m3"                          , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%leaf_carbon , &
                    vector_length          , &
                    "leaf_carbon"          , &
                    "leaf carbon mass"     , &
                    "g/m2"                 , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%root_carbon   , &
                    vector_length            , &
                    "root_carbon"            , &
                    "fine roots carbon mass" , &
                    "g/m2"                   , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%stem_carbon , &
                    vector_length          , &
                    "stem_carbon"          , &
                    "stem carbon mass"     , &
                    "g/m2"                 , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%wood_carbon                 , &
                    vector_length                          , &
                    "wood_carbon"                          , &
                    "wood (incl. woody roots) carbon mass" , &
                    "g/m2"                                 , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%soil_carbon_stable , &
                    vector_length                 , &
                    "soil_carbon_stable"          , &
                    "stable soil carbon mass"     , &
                    "g/m2"                        , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%soil_carbon_fast    , &
                    vector_length                  , &
                    "soil_carbon_fast"             , &
                    "short-lived soil carbon mass" , &
                    "g/m2"                         , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%grain_carbon , &
                    vector_length           , &
                    "grain_carbon"          , &
                    "grain carbon mass"     , &
                    "g/m2"                  , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%foliage_nitrogen , &
                    vector_length               , &
                    "foliage_nitrogen"          , &
                    "foliage nitrogen"          , &
                    "percent"                   , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%snow_water_equiv_old           , &
                    vector_length                             , &
                    "snow_water_equiv_old"                    , &
                    "snow water equivalent at last time step" , &
                    "mm"                                      , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%snow_depth , &
                    vector_length         , &
                    "snow_depth"          , &
                    "snow depth"          , &
                    "m"                   , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%state%snow_age        , &
                    vector_length              , &
                    "snow_age"                 , &
                    "non-dimensional snow age" , &
                    "-"                        , &
                    namelist%output_names, namelist%restart_names)
                    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Begin noahmp%flux variables

    call InitReal1d(this%flux%sw_absorbed_total      , &
                    vector_length                    , &
                    "sw_absorbed_total"              , &
                    "total absorbed solar radiation" , &
                    "W/m2"                           , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%sw_reflected_total      , &
                    vector_length                     , &
                    "sw_reflected_total"              , &
                    "total reflected solar radiation" , &
                    "W/m2"                            , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%lw_absorbed_total    , &
                    vector_length                  , &
                    "lw_absorbed_total"            , &
                    "total net longwave radiation" , &
                    "W/m2"                         , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%sensible_heat_total  , &
                    vector_length                  , &
                    "sensible_heat_total"          , &
                    "composite sensible heat flux" , &
                    "W/m2"                         , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%transpiration_heat    , &
                    vector_length                   , &
                    "transpiration_heat"            , &
                    "plant transpiration heat flux" , &
                    "W/m2"                          , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%latent_heat_canopy   , &
                    vector_length                  , &
                    "latent_heat_canopy"           , &
                    "canopy evaporation heat flux" , &
                    "W/m2"                         , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%latent_heat_ground   , &
                    vector_length                  , &
                    "latent_heat_ground"           , &
                    "direct soil latent heat flux" , &
                    "W/m2"                         , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%latent_heat_total   , &
                    vector_length                 , &
                    "latent_heat_total"           , &
                    "total grid latent heat flux" , &
                    "W/m2"                        , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%ground_heat_total    , &
                    vector_length                  , &
                    "ground_heat_total"            , &
                    "total heat flux into surface" , &
                    "W/m2"                         , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%precip_adv_heat_total         , &
                    vector_length                           , &
                    "precip_adv_heat_total"                 , &
                    "composite precipitation advected heat" , &
                    "W/m2"                                  , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%sw_absorbed_veg                , &
                    vector_length                            , &
                    "sw_absorbed_veg"                        , &
                    "solar radiation absorbed by vegetation" , &
                    "W/m2"                                   , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%sw_absorbed_ground         , &
                    vector_length                        , &
                    "sw_absorbed_ground"                 , &
                    "solar radiation absorbed by ground" , &
                    "W/m2"                               , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%lw_absorbed_grd_veg                     , &
                    vector_length                                     , &
                    "lw_absorbed_grd_veg"                             , &
                    "below-canopy ground absorbed longwave radiation" , &
                    "W/m2"                                            , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%lw_absorbed_leaf         , &
                    vector_length                      , &
                    "lw_absorbed_leaf"                 , &
                    "leaf absorbed longwave radiation" , &
                    "W/m2"                             , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%lw_absorbed_grd_bare       , &
                    vector_length                        , &
                    "lw_absorbed_grd_bare"               , &
                    "bare ground net longwave radiation" , &
                    "W/m2"                               , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%sensible_heat_grd_veg          , &
                    vector_length                            , &
                    "sensible_heat_grd_veg"                  , &
                    "below-canopy ground sensible heat flux" , &
                    "W/m2"                                   , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%sensible_heat_leaf        , &
                    vector_length                       , &
                    "sensible_heat_leaf"                , &
                    "leaf-to-canopy sensible heat flux" , &
                    "W/m2"                              , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%sensible_heat_grd_bar  , &
                    vector_length                    , &
                    "sensible_heat_grd_bar"          , &
                    "bare ground sensible heat flux" , &
                    "W/m2"                           , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%latent_heat_trans , &
                    vector_length               , &
                    "latent_heat_trans"         , &
                    "transpiration heat flux"   , &
                    "W/m2"                      , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%latent_heat_leaf   , &
                    vector_length                , &
                    "latent_heat_leaf"           , &
                    "leaf evaporation heat flux" , &
                    "W/m2"                       , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%latent_heat_grd_veg               , &
                    vector_length                               , &
                    "latent_heat_grd_veg"                       , &
                    "below-canopy ground evaporation heat flux" , &
                    "W/m2"                                      , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%latent_heat_grd_bare      , &
                    vector_length                       , &
                    "latent_heat_grd_bare"              , &
                    "bare ground evaporation heat flux" , &
                    "W/m2"                              , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%snow_sublimation , &
                    vector_length              , &
                    "snow_sublimation"         , &
                    "snow sublimation"         , &
                    "mm/s"                     , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%ground_heat_veg       , &
                    vector_length                   , &
                    "ground_heat_veg"               , &
                    "below-canopy ground heat flux" , &
                    "W/m2"                          , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%ground_heat_bare , &
                    vector_length              , &
                    "ground_heat_bare"         , &
                    "bare ground heat flux"    , &
                    "W/m2"                     , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%precip_adv_heat_veg              , &
                    vector_length                              , &
                    "precip_adv_heat_veg"                      , &
                    "precipitation advected heat - vegetation" , &
                    "W/m2"                                     , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%precip_adv_heat_grd_v              , &
                    vector_length                                , &
                    "precip_adv_heat_grd_v"                      , &
                    "precipitation advected heat - below-canopy" , &
                    "W/m2"                                       , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%precip_adv_heat_grd_b             , &
                    vector_length                               , &
                    "precip_adv_heat_grd_b"                     , &
                    "precipitation advected heat - bare ground" , &
                    "W/m2"                                      , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%transpiration    , &
                    vector_length              , &
                    "transpiration"            , &
                    "transpiration water flux" , &
                    "mm/s"                     , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%evaporation_canopy    , &
                    vector_length                   , &
                    "evaporation_canopy"            , &
                    "canopy evaporation water flux" , &
                    "mm/s"                          , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%evaporation_soil            , &
                    vector_length                         , &
                    "evaporation_soil"                    , &
                    "soil surface evaporation water flux" , &
                    "mm/s"                                , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%runoff_surface    , &
                    vector_length               , &
                    "runoff_surface"            , &
                    "surface runoff water flux" , &
                    "mm/s"                      , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%runoff_baseflow    , &
                    vector_length                , &
                    "runoff_baseflow"            , &
                    "drainage runoff water flux" , &
                    "mm/s"                       , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%snowmelt_out        , &
                    vector_length                 , &
                    "snowmelt_out"                , &
                    "snowmelt out bottom of pack" , &
                    "mm/s"                        , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%snowmelt_shallow , &
                    vector_length              , &
                    "snowmelt_shallow"         , &
                    "shallow snow melt"        , &
                    "mm/timestep"              , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%snowmelt_shallow_1   , &
                    vector_length                  , &
                    "snowmelt_shallow_1"           , &
                    "additional shallow snow melt" , &
                    "mm/timestep"                  , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%snowmelt_shallow_2   , &
                    vector_length                  , &
                    "snowmelt_shallow_2"           , &
                    "additional shallow snow melt" , &
                    "mm/timestep"                  , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%deep_recharge                         , &
                    vector_length                                   , &
                    "deep_recharge"                                 , &
                    "recharge to the water table when deep"         , &
                    "m"                                             , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%recharge                                 , &
                    vector_length                                      , &
                    "recharge"                                         , &
                    "recharge to the water table when shallow"         , &
                    "m"                                                , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%par_absorbed                     , &
                    vector_length                              , &
                    "par_absorbed"                             , &
                    "absorbed photosynthesis active radiation" , &
                    "W/m2"                                     , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%photosynthesis , &
                    vector_length            , &
                    "photosynthesis"         , &
                    "total photosynthesis"   , &
                    "umol CO2/m2/s"          , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%net_eco_exchange , &
                    vector_length              , &
                    "net_eco_exchange"         , &
                    "net ecosystem exchange"   , &
                    "g/m2/s CO2"               , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%global_prim_prod  , &
                    vector_length               , &
                    "global_prim_prod"          , &
                    "global primary production" , &
                    "g/m2/s C"                  , &
                    namelist%output_names, namelist%restart_names)

    call InitReal1d(this%flux%net_prim_prod    , &
                    vector_length              , &
                    "net_prim_prod"            , &
                    "net primary productivity" , &
                    "g/m2/s C"                 , &
                    namelist%output_names, namelist%restart_names)

! Set the default restart flags

    call this%InitRestart(namelist)

  end subroutine Init
  
  subroutine InitInt1d(indata, vector_length, in_name, in_longname, &
                       in_units, output_names, restart_names)
  
    type(int1d) :: indata
    integer :: vector_length, iname, numnames
    character(len=*)   :: in_name
    character(len=*)   :: in_longname
    character(len=*)   :: in_units
    character(len=128) :: output_names(200)
    character(len=128) :: restart_names(200)
    
    allocate(indata%data (vector_length))
    indata%data       = huge(1)
    indata%name       = in_name
    indata%long_name  = in_longname
    indata%units      = in_units
    
    numnames = count(output_names /= "")
    do iname = 1, numnames
      if(indata%name == output_names(iname)) indata%output_flag = .true.
    end do

    numnames = count(restart_names /= "")
    do iname = 1, numnames
      if(indata%name == restart_names(iname)) indata%restart_flag = .true.
    end do
  
  end subroutine InitInt1d

  subroutine InitReal1d(indata, vector_length, in_name, in_longname, &
                        in_units, output_names, restart_names)
  
    type(real1d) :: indata
    integer :: vector_length, iname, numnames
    character(len=*)   :: in_name
    character(len=*)   :: in_longname
    character(len=*)   :: in_units
    character(len=128) :: output_names(200)
    character(len=128) :: restart_names(200)
    
    allocate(indata%data (vector_length))
    indata%data       = huge(real(1.0,4))
    indata%name       = in_name
    indata%long_name  = in_longname
    indata%units      = in_units
  
    numnames = count(output_names /= "")
    do iname = 1, numnames
      if(indata%name == output_names(iname)) indata%output_flag = .true.
    end do

    numnames = count(restart_names /= "")
    do iname = 1, numnames
      if(indata%name == restart_names(iname)) indata%restart_flag = .true.
    end do
  
  end subroutine InitReal1d

  subroutine InitReal2d(indata, vector_length, ztop, zbot, in_name, in_longname,&
                        in_units, output_names, restart_names)
  
    type(real2d) :: indata
    integer :: vector_length, ztop, zbot, iname, numnames
    character(len=*)   :: in_name
    character(len=*)   :: in_longname
    character(len=*)   :: in_units
    character(len=128) :: output_names(200)
    character(len=128) :: restart_names(200)
    
    allocate(indata%data (vector_length, ztop:zbot))
    indata%data       = huge(real(1.0,4))
    indata%name       = in_name
    indata%long_name  = in_longname
    indata%units      = in_units
  
    numnames = count(output_names /= "")
    do iname = 1, numnames
      if(indata%name == output_names(iname)) indata%output_flag = .true.
    end do

    numnames = count(restart_names /= "")
    do iname = 1, numnames
      if(indata%name == restart_names(iname)) indata%restart_flag = .true.
    end do
  
  end subroutine InitReal2d

  subroutine InitRestart(this, namelist)

    class(noahmp_type)  :: this
    type(namelist_type) :: namelist
    
    this%model%vegetation_fraction%restart_flag    = .true.
    this%diag%emissivity_total%restart_flag        = .true.
    this%diag%albedo_direct%restart_flag           = .true.
    this%diag%albedo_diffuse%restart_flag          = .true.
    this%static%temperature_soil_bot%restart_flag  = .true.
    this%model%cm_noahmp%restart_flag              = .true.
    this%model%ch_noahmp%restart_flag              = .true.
    this%model%forcing_height%restart_flag         = .true.
    this%model%max_vegetation_frac%restart_flag    = .true.
    this%diag%albedo_total%restart_flag            = .true.
    this%state%snow_water_equiv%restart_flag       = .true.
    this%state%snow_depth%restart_flag             = .true.
    this%state%temperature_radiative%restart_flag  = .true.
    this%state%soil_moisture_vol%restart_flag      = .true.
    this%state%temperature_soil%restart_flag       = .true.
    this%state%soil_liquid_vol%restart_flag        = .true.
    this%diag%canopy_water%restart_flag            = .true.
    this%flux%transpiration_heat%restart_flag      = .true.
    this%model%friction_velocity%restart_flag      = .true.
    this%diag%z0_total%restart_flag                = .true.
    this%diag%snow_cover_fraction%restart_flag     = .true.
    this%diag%spec_humidity_surface%restart_flag   = .true.
    this%flux%ground_heat_total%restart_flag       = .true.
    this%flux%runoff_baseflow%restart_flag         = .true.
    this%flux%latent_heat_total%restart_flag       = .true.
    this%flux%sensible_heat_total%restart_flag     = .true.
    this%diag%evaporation_potential%restart_flag   = .true.
    this%flux%runoff_surface%restart_flag          = .true.
    this%flux%latent_heat_ground%restart_flag      = .true.
    this%flux%latent_heat_canopy%restart_flag      = .true.
    this%flux%snow_sublimation%restart_flag        = .true.
    this%diag%soil_moisture_total%restart_flag     = .true.
    this%flux%precip_adv_heat_total%restart_flag   = .true.
    this%model%cosine_zenith%restart_flag          = .true.
    this%model%snow_levels%restart_flag            = .true.
    this%state%temperature_leaf%restart_flag       = .true.
    this%state%temperature_ground%restart_flag     = .true.
    this%state%canopy_ice%restart_flag             = .true.
    this%state%canopy_liquid%restart_flag          = .true.
    this%state%vapor_pres_canopy_air%restart_flag  = .true.
    this%state%temperature_canopy_air%restart_flag = .true.
    this%diag%canopy_wet_fraction%restart_flag     = .true.
    this%state%snow_water_equiv_old%restart_flag   = .true.
    this%diag%snow_albedo_old%restart_flag         = .true.
    this%forcing%snowfall%restart_flag             = .true.
    this%state%lake_water%restart_flag             = .true.
    this%diag%depth_water_table%restart_flag       = .true.
    this%state%aquifer_water%restart_flag          = .true.
    this%state%saturated_water%restart_flag        = .true.
    this%state%leaf_carbon%restart_flag            = .true.
    this%state%root_carbon%restart_flag            = .true.
    this%state%stem_carbon%restart_flag            = .true.
    this%state%wood_carbon%restart_flag            = .true.
    this%state%soil_carbon_stable%restart_flag     = .true.
    this%state%soil_carbon_fast%restart_flag       = .true.
    this%model%leaf_area_index%restart_flag        = .true.
    this%model%stem_area_index%restart_flag        = .true.
    this%state%snow_age%restart_flag               = .true.
    this%state%soil_moisture_wtd%restart_flag      = .true.
    this%flux%deep_recharge%restart_flag           = .true.
    this%flux%recharge%restart_flag                = .true.
    this%diag%temperature_2m%restart_flag          = .true.
    this%diag%spec_humidity_2m%restart_flag        = .true.
    this%state%eq_soil_water_vol%restart_flag      = .true.
    this%state%temperature_snow%restart_flag       = .true.
    this%model%interface_depth%restart_flag        = .true.
    this%state%snow_level_ice%restart_flag         = .true.
    this%state%snow_level_liquid%restart_flag      = .true.
  
  end subroutine InitRestart

  subroutine TransferNamelist(this, namelist)
    
  class(noahmp_type)     :: this
  type(namelist_type)  :: namelist
  
  this%static%timestep                    = namelist%timestep_seconds
  this%model%forcing_height%data          = namelist%forcing_height
  this%options%dynamic_vegetation         = namelist%dynamic_vegetation_option
  this%options%canopy_stomatal_resistance = namelist%canopy_stomatal_resistance_option
  this%options%soil_wetness               = namelist%soil_wetness_option
  this%options%runoff                     = namelist%runoff_option
  this%options%surface_exchange           = namelist%surface_exchange_option
  this%options%supercooled_soilwater      = namelist%supercooled_soilwater_option
  this%options%frozen_soil_adjust         = namelist%frozen_soil_adjust_option
  this%options%radiative_transfer         = namelist%radiative_transfer_option
  this%options%snow_albedo                = namelist%snow_albedo_option
  this%options%precip_partition           = namelist%precip_partition_option
  this%options%soil_temp_lower_bdy        = namelist%soil_temp_lower_bdy_option
  this%options%soil_temp_time_scheme      = namelist%soil_temp_time_scheme_option
  this%options%thermal_roughness_scheme   = namelist%thermal_roughness_scheme_option
  this%options%surface_evap_resistance    = namelist%surface_evap_resistance_option
  this%options%glacier                    = namelist%glacier_option
  
  end subroutine TransferNamelist

  subroutine InitStates(this, namelist, now_time)
  
  ! out of necessity, this is extracted from FV3GFS_io.F90
  
  use time_utilities, only : date_from_since
  use noahmp_tables, only  : isbarren_table, isice_table, isurban_table, iswater_table, &
                             laim_table, saim_table, sla_table
    
  class(noahmp_type)   :: this
  type(namelist_type)  :: namelist

  double precision     :: now_time
  integer              :: current_mm
  character*19         :: current_date  ! format: yyyy-mm-dd hh:nn:ss
  real                 :: masslai, masssai, snow_depth_meters
  integer              :: ilevel, iloc, isnow

  call date_from_since("1970-01-01 00:00:00", now_time, current_date)
  read(current_date( 6: 7),  '(i2)') current_mm
  
  do iloc = 1, this%static%vector_length
  
    this%state%temperature_leaf%data      (iloc) = this%state%temperature_radiative%data(iloc)
    this%state%temperature_ground%data    (iloc) = this%state%temperature_radiative%data(iloc)
    this%state%temperature_canopy_air%data(iloc) = this%state%temperature_radiative%data(iloc)

    if (this%state%snow_depth%data(iloc) > 0.01 .and. this%state%temperature_radiative%data(iloc) > 273.15 ) then
      this%state%temperature_leaf%data      (iloc) = 273.15
      this%state%temperature_ground%data    (iloc) = 273.15
      this%state%temperature_canopy_air%data(iloc) = 273.15
    end if
  
    this%state%canopy_ice%data   (iloc) = 0.0
    this%state%canopy_liquid%data(iloc) = this%diag%canopy_water%data(iloc)

    this%state%vapor_pres_canopy_air%data(iloc) = 2000.0

    this%model%cm_noahmp%data            (iloc) = 0.0
    this%model%ch_noahmp%data            (iloc) = 0.0
    this%diag%canopy_wet_fraction%data   (iloc) = 0.0
    this%state%snow_water_equiv_old%data (iloc) = this%state%snow_water_equiv%data(iloc)
    this%diag%snow_albedo_old%data       (iloc) = 0.65
    this%forcing%snowfall%data           (iloc) = 0.0

    this%state%lake_water%data(iloc) = 0.0
    this%state%snow_age%data  (iloc) = 0.0

    this%state%aquifer_water%data      (iloc) = 4900.0                ! this assumes water table is at 2.5m
    this%state%saturated_water%data    (iloc) = this%state%aquifer_water%data(iloc)
    this%diag%depth_water_table%data   (iloc) = (25.0 + 2.0) - this%state%aquifer_water%data(iloc) / 1000.0 /0.2

    if ((this%static%vegetation_category%data(iloc) == isbarren_table) .or. &
        (this%static%vegetation_category%data(iloc) == isice_table   ) .or. &
        (this%static%vegetation_category%data(iloc) == isurban_table ) .or. &
        (this%static%vegetation_category%data(iloc) == iswater_table )) then

      this%model%leaf_area_index%data  (iloc) = 0.0
      this%model%stem_area_index%data  (iloc) = 0.0

      this%state%leaf_carbon%data(iloc) = 0.0
      this%state%stem_carbon%data(iloc) = 0.0
      this%state%root_carbon%data(iloc) = 0.0

      this%state%wood_carbon%data       (iloc) = 0.0       
      this%state%soil_carbon_stable%data(iloc) = 0.0      
      this%state%soil_carbon_fast%data  (iloc) = 0.0     

    else

      this%model%leaf_area_index%data  (iloc) = max(laim_table(this%static%vegetation_category%data(iloc), current_mm),0.05)
      this%model%stem_area_index%data  (iloc) = max(this%model%leaf_area_index%data(iloc)*0.1,0.05)

      masslai                           = 1000.0 / max(sla_table(this%static%vegetation_category%data(iloc)),1.0)
      this%state%leaf_carbon%data(iloc) = this%model%leaf_area_index%data(iloc) * masslai
      masssai                           = 1000.0 / 3.0
      this%state%stem_carbon%data(iloc) = this%model%stem_area_index%data(iloc) * masssai

      this%state%root_carbon%data       (iloc) = 500.0      
      this%state%wood_carbon%data       (iloc) = 500.0       
      this%state%soil_carbon_stable%data(iloc) = 1000.0      
      this%state%soil_carbon_fast%data  (iloc) = 1000.0     

    end if

    if (this%static%vegetation_category%data(iloc)  == isice_table )  then
      do ilevel = 1, namelist%num_soil_levels
        this%state%temperature_soil%data(iloc,ilevel) = &
           min(this%state%temperature_soil%data(iloc,ilevel),min(this%static%temperature_soil_bot%data(iloc),263.15))
      end do
      this%state%soil_liquid_vol%data  (iloc,:) = 0.0
      this%state%soil_moisture_vol%data(iloc,:) = 1.0
    end if

    snow_depth_meters = this%state%snow_depth%data(iloc) / 1000.0  ! snow depth in meters

    if (this%state%snow_water_equiv%data(iloc) /= 0.0 .and. snow_depth_meters == 0.0 ) then
      snow_depth_meters = this%state%snow_water_equiv%data(iloc) / 1000.0 * 10.0   ! assume 10/1 ratio
    endif

    if (this%static%vegetation_category%data(iloc) == isice_table) then  ! land ice in MODIS/IGBP
      if (this%state%snow_water_equiv%data(iloc) < 0.1) then
        this%state%snow_water_equiv%data(iloc) = 0.1
        snow_depth_meters = this%state%snow_water_equiv%data(iloc) / 1000.0 * 10.0 
      end if
    end if

    this%model%snow_soil_thickness%data = 0.0
    if (snow_depth_meters < 0.025 ) then
      this%model%snow_levels%data(iloc)              = 0.0
      this%model%snow_soil_thickness%data(iloc,-2:0) = 0.0
    elseif (snow_depth_meters >= 0.025 .and. snow_depth_meters <= 0.05 ) then
      this%model%snow_levels%data(iloc)              = -1.0
      this%model%snow_soil_thickness%data(iloc,0)    = snow_depth_meters
    elseif (snow_depth_meters > 0.05 .and. snow_depth_meters <= 0.10 ) then
      this%model%snow_levels%data(iloc)              = -2.0
      this%model%snow_soil_thickness%data(iloc,-1)   = 0.5*snow_depth_meters
      this%model%snow_soil_thickness%data(iloc,0)    = 0.5*snow_depth_meters
    elseif (snow_depth_meters > 0.10 .and. snow_depth_meters <= 0.25 ) then
      this%model%snow_levels%data(iloc)                   = -2.0
      this%model%snow_soil_thickness%data(iloc,-1)   = 0.05
      this%model%snow_soil_thickness%data(iloc,0)    = snow_depth_meters - 0.05
    elseif (snow_depth_meters > 0.25 .and. snow_depth_meters <= 0.45 ) then
      this%model%snow_levels%data(iloc)                   = -3.0
      this%model%snow_soil_thickness%data(iloc,-2)   = 0.05
      this%model%snow_soil_thickness%data(iloc,-1)   = 0.5*(snow_depth_meters-0.05)
      this%model%snow_soil_thickness%data(iloc,0)    = 0.5*(snow_depth_meters-0.05)
    elseif (snow_depth_meters > 0.45) then 
      this%model%snow_levels%data(iloc)                   = -3.0
      this%model%snow_soil_thickness%data(iloc,-2)   = 0.05
      this%model%snow_soil_thickness%data(iloc,-1)   = 0.20
      this%model%snow_soil_thickness%data(iloc,0)    = snow_depth_meters - 0.05 - 0.20
    else
      ! call mpp_error(FATAL, 'problem with the logic assigning snow layers.') 
    endif

! Now we have the snow_levels field
! snice + snliq + tsno allocation and compute them from what we have
             
    this%state%temperature_snow%data (iloc,-2:0) = 0.0
    this%state%snow_level_ice%data   (iloc,-2:0) = 0.0
    this%state%snow_level_liquid%data(iloc,-2:0) = 0.0
    this%model%interface_depth%data  (iloc,-2:namelist%num_soil_levels) = 0.0

    isnow = nint(this%model%snow_levels%data(iloc)) + 1    ! snow_levels <=0.0, interface_depth >= 0.0

    do ilevel = isnow , 0
      this%state%temperature_snow%data (iloc,ilevel) = this%state%temperature_ground%data(iloc)
      this%state%snow_level_liquid%data(iloc,ilevel) = 0.0
      this%state%snow_level_ice%data(iloc,ilevel)    = this%model%snow_soil_thickness%data(iloc,-2) / snow_depth_meters &
                                                        * this%state%snow_water_equiv%data(iloc)
    end do

    this%model%interface_depth%data(iloc,isnow) = -1.0*this%model%snow_soil_thickness%data(iloc,isnow)
    do ilevel = isnow+1, 0
      this%model%interface_depth%data(iloc,ilevel) = this%model%interface_depth%data(iloc,ilevel-1) - &
                                                 this%model%snow_soil_thickness%data(iloc,ilevel)
    end do
    do ilevel = 1, namelist%num_soil_levels
      this%model%snow_soil_thickness%data(iloc,ilevel) = namelist%soil_level_thickness(ilevel)
      this%model%interface_depth%data(iloc,ilevel)     = this%model%interface_depth%data(iloc,ilevel-1) - &
                                                     namelist%soil_level_thickness(ilevel)
    end do

! Should not need to initialize these
    
    this%state%eq_soil_water_vol%data(iloc,:) = 0.0
    this%state%soil_moisture_wtd%data(iloc)   = 0.0
    this%flux%deep_recharge%data(iloc)        = 0.0
    this%flux%recharge%data(iloc)             = 0.0

  end do ! iloc
  
  end subroutine InitStates

end module ufsLandNoahMPType
