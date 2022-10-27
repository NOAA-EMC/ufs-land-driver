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

  type(real1d)  :: air_pressure_forcing     ! forcing air pressure [Pa]
  type(real1d)  :: precip_convective        ! convective precipitation [mm/s]
  type(real1d)  :: precip_non_convective    ! non-convective precipitation [mm/s]
  type(real1d)  :: precip_snow              ! snow precipitation [mm/s]
  type(real1d)  :: precip_graupel           ! graupel precipitation [mm/s]
  type(real1d)  :: precip_hail              ! hail precipitation [mm/s]
  type(real1d)  :: snowfall                 ! land model partitioned snowfall [mm/s]
  type(real1d)  :: rainfall                 ! land model partitioned rainfall [mm/s]

end type noahmp_forcing_type

type :: noahmp_diag_type

  type(real1d)  :: z0_total               ! weighted z0 sent to coupled model [m]
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
  type(real1d)  :: vegetation_fraction    ! vegetation fraction [0.0-1.0]
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

  type (noahmp_static_type)  :: static
  type (noahmp_options_type) :: options
  type (noahmp_model_type)   :: model
  type (noahmp_forcing_type) :: forcing
  type (noahmp_diag_type)    :: diag
  type (noahmp_state_type)   :: state
  type (noahmp_flux_type)    :: flux

  contains

    procedure, public  :: Init         
    procedure, private :: InitAllocate 
    procedure, private :: InitDefault     
    procedure, public  :: TransferNamelist         
    procedure, public  :: InitStates         

end type noahmp_type

contains   

  subroutine Init(this, namelist, vector_length)

    class(noahmp_type) :: this
    type(namelist_type) :: namelist
    integer :: vector_length

    call this%InitAllocate(namelist,vector_length)
    call this%InitDefault()

  end subroutine Init

  subroutine InitAllocate(this, namelist, vector_length)

    class(noahmp_type) :: this
    type(namelist_type) :: namelist
    integer :: vector_length, soil_levels, snow_index

    this%static%vector_length   = vector_length
    this%static%soil_levels     = namelist%num_soil_levels
    this%static%max_snow_levels = namelist%num_snow_levels
    soil_levels = namelist%num_soil_levels
    snow_index = -1*namelist%num_snow_levels + 1

    allocate(this%static%vegetation_category%data    (vector_length)            ) ! vegetation type (integer index)
    allocate(this%static%soil_category%data          (vector_length)            ) ! soil texture type (integer index)
    allocate(this%static%slope_category%data         (vector_length)            ) ! slope category (integer index)
    allocate(this%static%soil_interface_depth%data   (vector_length)            ) ! soil layer-bottom depth from surface [m]
    allocate(this%static%ice_flag%data               (vector_length)            ) ! ice flag (1->ice)
    allocate(this%static%surface_type%data           (vector_length)            ) ! surface type flag 1->soil; 2->lake
    allocate(this%static%crop_type%data              (vector_length)            ) ! crop type category
    allocate(this%static%temperature_soil_bot%data   (vector_length)            ) ! soil bottom boundary condition temperature [K]

    allocate(this%model%latitude%data                (vector_length)            ) ! latitude [radians]
    allocate(this%model%cosine_zenith%data           (vector_length)            ) ! cosine solar zenith angle [-1,1]
    allocate(this%model%forcing_height%data          (vector_length)            ) ! forcing height [m]
    allocate(this%model%vegetation_fraction%data     (vector_length)            ) ! vegetation fraction [0.0-1.0]
    allocate(this%model%max_vegetation_frac%data     (vector_length)            ) ! annual maximum vegetation fraction [0.0-1.0]
    allocate(this%model%snow_levels%data             (vector_length)            ) ! active snow levels [-]
    allocate(this%model%interface_depth%data         (vector_length,snow_index:soil_levels)) ! layer-bottom depth from snow surf [m]
    allocate(this%model%snow_soil_thickness%data     (vector_length,snow_index:soil_levels)) ! thickness of each snow/soil level [m]
    allocate(this%model%leaf_area_index%data         (vector_length)            ) ! leaf area index [-]
    allocate(this%model%stem_area_index%data         (vector_length)            ) ! stem area index [-]
    allocate(this%model%growing_deg_days%data        (vector_length)            ) ! growing degree days [-]
    allocate(this%model%plant_growth_stage%data      (vector_length)            ) ! plant growing stage [-]
    allocate(this%model%cm_noahmp%data               (vector_length)            ) ! grid momentum drag coefficient [m/s]
    allocate(this%model%ch_noahmp%data               (vector_length)            ) ! grid heat exchange coefficient [m/s]
    allocate(this%model%ch_vegetated%data            (vector_length)            ) ! vegetated heat exchange coefficient [m/s]
    allocate(this%model%ch_bare_ground%data          (vector_length)            ) ! bare-ground heat exchange coefficient [m/s]
    allocate(this%model%ch_leaf%data                 (vector_length)            ) ! leaf exchange coefficient [m/s]
    allocate(this%model%ch_below_canopy%data         (vector_length)            ) ! below-canopy exchange coefficient [m/s]
    allocate(this%model%ch_vegetated_2m%data         (vector_length)            ) ! 2-m vegetated  heat exchange coefficient [m/s]
    allocate(this%model%ch_bare_ground_2m%data       (vector_length)            ) ! 2-m bare-ground heat exchange coefficient [m/s]
    allocate(this%model%friction_velocity%data       (vector_length)            ) ! friction velocity [m/s]
    allocate(this%model%rs_sunlit%data               (vector_length)            ) ! sunlit leaf stomatal resistance [s/m]
    allocate(this%model%rs_shaded%data               (vector_length)            ) ! shaded leaf stomatal resistance [s/m]
    allocate(this%model%leaf_air_resistance%data     (vector_length)            ) ! leaf boundary layer resistance [s/m]
    allocate(this%model%pbl_height%data              (vector_length)            ) ! height of pbl [m]
    allocate(this%model%mo_length_inverse%data       (vector_length)            ) ! reciprocle of M-O length [1/m]
    allocate(this%model%heat_flux_multiplier%data    (vector_length)            ) ! heat flux multiplier [W/m^2/K]
    allocate(this%model%moisture_flux_multiplier%data(vector_length)            ) ! moisture flux multiplier [kg/m^2/s]

    allocate(this%forcing%air_pressure_forcing%data  (vector_length)            ) ! forcing air pressure [Pa]
    allocate(this%forcing%precip_convective%data     (vector_length)            ) ! convective precipitation [mm/s]
    allocate(this%forcing%precip_non_convective%data (vector_length)            ) ! non-convective precipitation [mm/s]
    allocate(this%forcing%precip_snow%data           (vector_length)            ) ! snow precipitation [mm/s]
    allocate(this%forcing%precip_graupel%data        (vector_length)            ) ! graupel precipitation [mm/s]
    allocate(this%forcing%precip_hail%data           (vector_length)            ) ! hail precipitation [mm/s]
    allocate(this%forcing%snowfall%data              (vector_length)            ) ! land model partitioned snowfall [mm/s]
    allocate(this%forcing%rainfall%data              (vector_length)            ) ! land model partitioned rainfall [mm/s]

    allocate(this%diag%z0_total%data                 (vector_length)            ) ! weighted z0 sent to coupled model [m]
    allocate(this%diag%albedo_total%data             (vector_length)            ) ! total surface albedo [-]
    allocate(this%diag%albedo_direct%data            (vector_length,2)          ) ! direct vis/nir albedo [-]
    allocate(this%diag%albedo_diffuse%data           (vector_length,2)          ) ! diffuse vis/nir albedo [-]
    allocate(this%diag%albedo_direct_snow%data       (vector_length,2)          ) ! direct vis/nir snow albedo [-]
    allocate(this%diag%albedo_diffuse_snow%data      (vector_length,2)          ) ! diffuse vis/nir snow albedo [-]
    allocate(this%diag%emissivity_total%data         (vector_length)            ) ! grid emissivity [-]
    allocate(this%diag%canopy_gap_fraction%data      (vector_length)            ) ! between canopy gap fraction [-]
    allocate(this%diag%incanopy_gap_fraction%data    (vector_length)            ) ! within canopy gap fraction for beam [-]
    allocate(this%diag%precip_frozen_frac%data       (vector_length)            ) ! precipitation snow fraction [-]
    allocate(this%diag%snow_cover_fraction%data      (vector_length)            ) ! snow cover fraction on the ground [-]
    allocate(this%diag%vegetation_fraction%data      (vector_length)            ) ! vegetation fraction [0.0-1.0]
    allocate(this%diag%canopy_wet_fraction%data      (vector_length)            ) ! wetted or snowed fraction of canopy [-]
    allocate(this%diag%canopy_water%data             (vector_length)            ) ! canopy-intercepted water [mm]
    allocate(this%diag%depth_water_table%data        (vector_length)            ) ! depth to water table [m]
    allocate(this%diag%lai_sunlit%data               (vector_length)            ) ! sunlit leaf area index [m2/m2]
    allocate(this%diag%lai_shaded%data               (vector_length)            ) ! shaded leaf area index [m2/m2]
    allocate(this%diag%snow_ice_frac_old%data        (vector_length,snow_index:0)) ! snow ice fraction at last timestep [-]
    allocate(this%diag%snow_albedo_old%data          (vector_length)            ) ! snow albedo at last time step [-]
    allocate(this%diag%evaporation_potential%data    (vector_length)            ) ! potential evaporation [mm/s]
    allocate(this%diag%soil_moisture_total%data      (vector_length)            ) ! total soil moisture in all levels [mm]
    allocate(this%diag%temperature_veg_2m%data       (vector_length)            ) ! vegetated 2-m air temperature [K]
    allocate(this%diag%temperature_bare_2m%data      (vector_length)            ) ! bare ground 2-m air temperature [K]
    allocate(this%diag%temperature_2m%data           (vector_length)            ) ! composite 2-m air temperature [K]
    allocate(this%diag%spec_humidity_veg_2m%data     (vector_length)            ) ! vegetated 2-m air specific humidity [K]
    allocate(this%diag%spec_humidity_bare_2m%data    (vector_length)            ) ! bare ground 2-m air specfic humidity [K]
    allocate(this%diag%spec_humidity_2m%data         (vector_length)            ) ! composite 2-m air specfic humidity [K]
    allocate(this%diag%spec_humidity_surface%data    (vector_length)            ) ! surface specific humidty [kg/kg]

    allocate(this%state%temperature_soil%data        (vector_length,soil_levels)) ! soil temperature [K]
    allocate(this%state%temperature_snow%data        (vector_length,snow_index:0)) ! snow temperature [K]
    allocate(this%state%temperature_canopy_air%data  (vector_length)            ) ! canopy air temperature [K]
    allocate(this%state%temperature_radiative%data   (vector_length)            ) ! surface radiative temperature [K]
    allocate(this%state%temperature_leaf%data        (vector_length)            ) ! leaf temperature [K]
    allocate(this%state%temperature_ground%data      (vector_length)            ) ! grid ground surface temperature [K]
    allocate(this%state%temperature_bare_grd%data    (vector_length)            ) ! bare ground surface temperature [K]
    allocate(this%state%temperature_veg_grd%data     (vector_length)            ) ! below_canopy ground surface temperature [K]
    allocate(this%state%vapor_pres_canopy_air%data   (vector_length)            ) ! canopy air vapor pressure [Pa]
    allocate(this%state%soil_liquid_vol%data         (vector_length,soil_levels)) ! volumetric liquid soil moisture [m3/m3]
    allocate(this%state%soil_moisture_vol%data       (vector_length,soil_levels)) ! volumetric soil moisture (ice + liq.) [m3/m3]
    allocate(this%state%snow_water_equiv%data        (vector_length)            ) ! snow water equivalent [kg/m2]
    allocate(this%state%snow_level_ice%data          (vector_length,snow_index:0)) ! snow level ice [mm]
    allocate(this%state%snow_level_liquid%data       (vector_length,snow_index:0)) ! snow level liquid [mm]
    allocate(this%state%canopy_liquid%data           (vector_length)            ) ! canopy-intercepted liquid [mm]
    allocate(this%state%canopy_ice%data              (vector_length)            ) ! canopy-intercepted ice [mm]
    allocate(this%state%aquifer_water%data           (vector_length)            ) ! water storage in aquifer [mm]
    allocate(this%state%saturated_water%data         (vector_length)            ) ! water in aquifer+saturated soil [mm]
    allocate(this%state%lake_water%data              (vector_length)            ) ! lake water storage [mm]
    allocate(this%state%soil_moisture_wtd%data       (vector_length)            ) ! (opt_run=5) soil water content between bottom of the soil and water table [m3/m3]
    allocate(this%state%eq_soil_water_vol%data       (vector_length,soil_levels)) ! (opt_run=5) equilibrium soil water content [m3/m3]
    allocate(this%state%leaf_carbon%data             (vector_length)            ) ! leaf mass [g/m2]
    allocate(this%state%root_carbon%data             (vector_length)            ) ! mass of fine roots [g/m2]
    allocate(this%state%stem_carbon%data             (vector_length)            ) ! stem mass [g/m2]
    allocate(this%state%wood_carbon%data             (vector_length)            ) ! mass of wood (incl. woody roots) [g/m2]
    allocate(this%state%soil_carbon_stable%data      (vector_length)            ) ! stable soil carbon [g/m2]
    allocate(this%state%soil_carbon_fast%data        (vector_length)            ) ! short-lived soil carbon [g/m2]
    allocate(this%state%grain_carbon%data            (vector_length)            ) ! grain mass [g/m2]
    allocate(this%state%foliage_nitrogen%data        (vector_length)            ) ! foliage nitrogen [%] [1-saturated]
    allocate(this%state%snow_water_equiv_old%data    (vector_length)            ) ! snow water equivalent at last time step [kg/m2]
    allocate(this%state%snow_depth%data              (vector_length)            ) ! snow depth [m]
    allocate(this%state%snow_age%data                (vector_length)            ) ! non-dimensional snow age [-]

    allocate(this%flux%sw_absorbed_total%data        (vector_length)            ) ! total absorbed solar radiation [W/m2]
    allocate(this%flux%sw_reflected_total%data       (vector_length)            ) ! total reflected solar radiation [W/m2]
    allocate(this%flux%lw_absorbed_total%data        (vector_length)            ) ! total net lw rad [W/m2]  [+ to atm]
    allocate(this%flux%sensible_heat_total%data      (vector_length)            ) ! total sensible heat [W/m2] [+ to atm]
    allocate(this%flux%transpiration_heat%data       (vector_length)            ) ! transpiration heat flux [W/m2] [+ to atm]
    allocate(this%flux%latent_heat_canopy%data       (vector_length)            ) ! canopy evaporation heat flux [W/m2] [+ to atm]
    allocate(this%flux%latent_heat_ground%data       (vector_length)            ) ! ground evaporation heat flux [W/m2] [+ to atm]
    allocate(this%flux%latent_heat_total%data        (vector_length)            ) ! total latent heat flux [W/m2] [+ to atm]
    allocate(this%flux%ground_heat_total%data        (vector_length)            ) ! ground heat flux [W/m2]   [+ to soil]
    allocate(this%flux%precip_adv_heat_total%data    (vector_length)            ) ! precipitation advected heat - total [W/m2)
    allocate(this%flux%sw_absorbed_veg%data          (vector_length)            ) ! solar radiation absorbed by vegetation [W/m2]
    allocate(this%flux%sw_absorbed_ground%data       (vector_length)            ) ! solar radiation absorbed by ground [W/m2]
    allocate(this%flux%lw_absorbed_grd_veg%data      (vector_length)            ) ! below-canopy ground absorbed longwave radiation [W/m2]
    allocate(this%flux%lw_absorbed_leaf%data         (vector_length)            ) ! leaf absorbed longwave radiation [W/m2]
    allocate(this%flux%lw_absorbed_grd_bare%data     (vector_length)            ) ! bare ground net longwave radiation [W/m2]
    allocate(this%flux%sensible_heat_grd_veg%data    (vector_length)            ) ! below-canopy ground sensible heat flux [W/m2]
    allocate(this%flux%sensible_heat_leaf%data       (vector_length)            ) ! leaf-to-canopy sensible heat flux [W/m2]
    allocate(this%flux%sensible_heat_grd_bar%data    (vector_length)            ) ! bare ground sensible heat flux [W/m2]
    allocate(this%flux%latent_heat_trans%data        (vector_length)            ) ! transpiration [W/m2]
    allocate(this%flux%latent_heat_leaf%data         (vector_length)            ) ! leaf evaporation [W/m2]
    allocate(this%flux%latent_heat_grd_veg%data      (vector_length)            ) ! below-canopy ground evaporation heat flux [W/m2]
    allocate(this%flux%latent_heat_grd_bare%data     (vector_length)            ) ! bare ground evaporation heat flux [W/m2]
    allocate(this%flux%snow_sublimation%data         (vector_length)            ) ! snow sublimation [W/m2]
    allocate(this%flux%ground_heat_veg%data          (vector_length)            ) ! below-canopy ground heat flux [W/m2]
    allocate(this%flux%ground_heat_bare%data         (vector_length)            ) ! bare ground heat flux [W/m2]
    allocate(this%flux%precip_adv_heat_veg%data      (vector_length)            ) ! precipitation advected heat - vegetation net [W/m2]
    allocate(this%flux%precip_adv_heat_grd_v%data    (vector_length)            ) ! precipitation advected heat - below-canopy net [W/m2]
    allocate(this%flux%precip_adv_heat_grd_b%data    (vector_length)            ) ! precipitation advected heat - bare ground net [W/m2]
    allocate(this%flux%transpiration%data            (vector_length)            ) ! transpiration [mm/s]
    allocate(this%flux%evaporation_canopy%data       (vector_length)            ) ! canopy evaporation [mm/s]
    allocate(this%flux%evaporation_soil%data         (vector_length)            ) ! soil surface evaporation [mm/s]
    allocate(this%flux%runoff_surface%data           (vector_length)            ) ! surface runoff [mm/s] 
    allocate(this%flux%runoff_baseflow%data          (vector_length)            ) ! baseflow runoff [mm/s]
    allocate(this%flux%snowmelt_out%data             (vector_length)            ) ! snowmelt out bottom of pack [mm/s]
    allocate(this%flux%snowmelt_shallow%data         (vector_length)            ) ! shallow snow melt [mm/timestep]
    allocate(this%flux%snowmelt_shallow_1%data       (vector_length)            ) ! additional shallow snow melt [mm/timestep]
    allocate(this%flux%snowmelt_shallow_2%data       (vector_length)            ) ! additional shallow snow melt [mm/timestep]
    allocate(this%flux%deep_recharge%data            (vector_length)            ) ! (opt_run=5) recharge to or from the water table when deep [m]
    allocate(this%flux%recharge%data                 (vector_length)            ) ! (opt_run=5) recharge to or from the water table when shallow [m]
    allocate(this%flux%par_absorbed%data             (vector_length)            ) ! absorbed photosynthesis active radiation [W/m2]
    allocate(this%flux%photosynthesis%data           (vector_length)            ) ! total photosynthesis [umol CO2/m2/s] [+ out]
    allocate(this%flux%net_eco_exchange%data         (vector_length)            ) ! net ecosystem exchange [g/m2/s CO2]
    allocate(this%flux%global_prim_prod%data         (vector_length)            ) ! global primary production [g/m2/s C]
    allocate(this%flux%net_prim_prod%data            (vector_length)            ) ! net primary productivity [g/m2/s C]

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(noahmp_type) :: this

    this%static%vegetation_category%data    = huge(1)
    this%static%soil_category%data          = huge(1)
    this%static%slope_category%data         = huge(1)
    this%static%soil_interface_depth%data   = huge(1.0)
    this%static%ice_flag%data               = huge(1)
    this%static%surface_type%data           = huge(1)
    this%static%crop_type%data              = huge(1)
    this%static%temperature_soil_bot%data   = huge(1.0)

    this%model%latitude%data                = huge(1.0)
    this%model%cosine_zenith%data           = huge(1.0)
    this%model%forcing_height%data          = huge(1.0)
    this%model%vegetation_fraction%data     = huge(1.0)
    this%model%max_vegetation_frac%data     = huge(1.0)
    this%model%snow_levels%data             = huge(1.0)
    this%model%interface_depth%data         = huge(1.0)
    this%model%snow_soil_thickness%data     = huge(1.0)
    this%model%leaf_area_index%data         = huge(1.0)
    this%model%stem_area_index%data         = huge(1.0)
    this%model%growing_deg_days%data        = huge(1.0)
    this%model%plant_growth_stage%data      = huge(1)
    this%model%cm_noahmp%data               = huge(1.0)
    this%model%ch_noahmp%data               = huge(1.0)
    this%model%ch_vegetated%data            = huge(1.0)
    this%model%ch_bare_ground%data          = huge(1.0)
    this%model%ch_leaf%data                 = huge(1.0)
    this%model%ch_below_canopy%data         = huge(1.0)
    this%model%ch_vegetated_2m%data         = huge(1.0)
    this%model%ch_bare_ground_2m%data       = huge(1.0)
    this%model%friction_velocity%data       = huge(1.0)
    this%model%rs_sunlit%data               = huge(1.0)
    this%model%rs_shaded%data               = huge(1.0)
    this%model%leaf_air_resistance%data     = huge(1.0)
    this%model%pbl_height%data              = huge(1.0)
    this%model%mo_length_inverse%data       = huge(1.0)
    this%model%heat_flux_multiplier%data    = huge(1.0)
    this%model%moisture_flux_multiplier%data= huge(1.0)

    this%forcing%air_pressure_forcing%data  = huge(1.0)
    this%forcing%precip_convective%data     = huge(1.0)
    this%forcing%precip_non_convective%data = huge(1.0)
    this%forcing%precip_snow%data           = huge(1.0)
    this%forcing%precip_graupel%data        = huge(1.0)
    this%forcing%precip_hail%data           = huge(1.0)
    this%forcing%snowfall%data              = huge(1.0)
    this%forcing%rainfall%data              = huge(1.0)

    this%diag%z0_total%data                 = huge(1.0)
    this%diag%albedo_total%data             = huge(1.0)
    this%diag%albedo_direct%data            = huge(1.0)
    this%diag%albedo_diffuse%data           = huge(1.0)
    this%diag%albedo_direct_snow%data       = huge(1.0)
    this%diag%albedo_diffuse_snow%data      = huge(1.0)
    this%diag%emissivity_total%data         = huge(1.0)
    this%diag%canopy_gap_fraction%data      = huge(1.0)
    this%diag%incanopy_gap_fraction%data    = huge(1.0)
    this%diag%precip_frozen_frac%data       = huge(1.0)
    this%diag%snow_cover_fraction%data      = huge(1.0)
    this%diag%vegetation_fraction%data      = huge(1.0)
    this%diag%canopy_wet_fraction%data      = huge(1.0)
    this%diag%canopy_water%data             = huge(1.0)
    this%diag%depth_water_table%data        = huge(1.0)
    this%diag%lai_sunlit%data               = huge(1.0)
    this%diag%lai_shaded%data               = huge(1.0)
    this%diag%snow_ice_frac_old%data        = huge(1.0)
    this%diag%snow_albedo_old%data          = huge(1.0)
    this%diag%evaporation_potential%data    = huge(1.0)
    this%diag%soil_moisture_total%data      = huge(1.0)
    this%diag%temperature_veg_2m%data       = huge(1.0)
    this%diag%temperature_bare_2m%data      = huge(1.0)
    this%diag%temperature_2m%data           = huge(1.0)
    this%diag%spec_humidity_veg_2m%data     = huge(1.0)
    this%diag%spec_humidity_bare_2m%data    = huge(1.0)
    this%diag%spec_humidity_2m%data         = huge(1.0)
    this%diag%spec_humidity_surface%data    = huge(1.0)

    this%state%temperature_soil%data        = huge(1.0)
    this%state%temperature_snow%data        = huge(1.0)
    this%state%temperature_canopy_air%data  = huge(1.0)
    this%state%temperature_radiative%data   = huge(1.0)
    this%state%temperature_leaf%data        = huge(1.0)
    this%state%temperature_ground%data      = huge(1.0)
    this%state%temperature_bare_grd%data    = huge(1.0)
    this%state%temperature_veg_grd%data     = huge(1.0)
    this%state%vapor_pres_canopy_air%data   = huge(1.0)
    this%state%soil_liquid_vol%data         = huge(1.0)
    this%state%soil_moisture_vol%data       = huge(1.0)
    this%state%snow_water_equiv%data        = huge(1.0)
    this%state%snow_level_ice%data          = huge(1.0)
    this%state%snow_level_liquid%data       = huge(1.0)
    this%state%canopy_liquid%data           = huge(1.0)
    this%state%canopy_ice%data              = huge(1.0)
    this%state%aquifer_water%data           = huge(1.0)
    this%state%saturated_water%data         = huge(1.0)
    this%state%lake_water%data              = huge(1.0)
    this%state%soil_moisture_wtd%data       = huge(1.0)
    this%state%eq_soil_water_vol%data       = huge(1.0)
    this%state%leaf_carbon%data             = huge(1.0)
    this%state%root_carbon%data             = huge(1.0)
    this%state%stem_carbon%data             = huge(1.0)
    this%state%wood_carbon%data             = huge(1.0)
    this%state%soil_carbon_stable%data      = huge(1.0)
    this%state%soil_carbon_fast%data        = huge(1.0)
    this%state%grain_carbon%data            = huge(1.0)
    this%state%foliage_nitrogen%data        = huge(1.0)
    this%state%snow_water_equiv_old%data    = huge(1.0)
    this%state%snow_depth%data              = huge(1.0)
    this%state%snow_age%data                = huge(1.0)

    this%flux%sw_absorbed_total%data        = huge(1.0)
    this%flux%sw_reflected_total%data       = huge(1.0)
    this%flux%lw_absorbed_total%data        = huge(1.0)
    this%flux%sensible_heat_total%data      = huge(1.0)
    this%flux%transpiration_heat%data       = huge(1.0)
    this%flux%latent_heat_canopy%data       = huge(1.0)
    this%flux%latent_heat_ground%data       = huge(1.0)
    this%flux%latent_heat_total%data        = huge(1.0)
    this%flux%ground_heat_total%data        = huge(1.0)
    this%flux%precip_adv_heat_total%data    = huge(1.0)
    this%flux%sw_absorbed_veg%data          = huge(1.0)
    this%flux%sw_absorbed_ground%data       = huge(1.0)
    this%flux%lw_absorbed_grd_veg%data      = huge(1.0)
    this%flux%lw_absorbed_leaf%data         = huge(1.0)
    this%flux%lw_absorbed_grd_bare%data     = huge(1.0)
    this%flux%sensible_heat_grd_veg%data    = huge(1.0)
    this%flux%sensible_heat_leaf%data       = huge(1.0)
    this%flux%sensible_heat_grd_bar%data    = huge(1.0)
    this%flux%latent_heat_trans%data        = huge(1.0)
    this%flux%latent_heat_leaf%data         = huge(1.0)
    this%flux%latent_heat_grd_veg%data      = huge(1.0)
    this%flux%latent_heat_grd_bare%data     = huge(1.0)
    this%flux%snow_sublimation%data         = huge(1.0)
    this%flux%ground_heat_veg%data          = huge(1.0)
    this%flux%ground_heat_bare%data         = huge(1.0)
    this%flux%precip_adv_heat_veg%data      = huge(1.0)
    this%flux%precip_adv_heat_grd_v%data    = huge(1.0)
    this%flux%precip_adv_heat_grd_b%data    = huge(1.0)
    this%flux%transpiration%data            = huge(1.0)
    this%flux%evaporation_canopy%data       = huge(1.0)
    this%flux%evaporation_soil%data         = huge(1.0)
    this%flux%runoff_surface%data           = huge(1.0)
    this%flux%runoff_baseflow%data          = huge(1.0)
    this%flux%snowmelt_out%data             = huge(1.0)
    this%flux%snowmelt_shallow%data         = huge(1.0)
    this%flux%snowmelt_shallow_1%data       = huge(1.0)
    this%flux%snowmelt_shallow_2%data       = huge(1.0)
    this%flux%deep_recharge%data            = huge(1.0)
    this%flux%recharge%data                 = huge(1.0)
    this%flux%par_absorbed%data             = huge(1.0)
    this%flux%photosynthesis%data           = huge(1.0)
    this%flux%net_eco_exchange%data         = huge(1.0)
    this%flux%global_prim_prod%data         = huge(1.0)
    this%flux%net_prim_prod%data            = huge(1.0)

  end subroutine InitDefault

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
