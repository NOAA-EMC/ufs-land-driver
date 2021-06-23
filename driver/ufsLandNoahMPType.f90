module ufsLandNoahMPType

use NamelistRead
use machine , only : kind_phys

implicit none
save
private

type :: noahmp_static_type

  real(kind=kind_phys)              :: timestep                ! time interval [s]
  integer                           :: vector_length           ! number of simulation grids
  integer                           :: soil_source             ! soil type data source zobler or statsgo
  integer                           :: veg_source              ! veg type data source umd or igbp
  integer                           :: soil_levels             ! vertical soil layer dimension
  integer                           :: max_snow_levels         ! maximum number of snow levels
  integer             , allocatable :: vegetation_category (:) ! vegetation type (integer index)
  integer             , allocatable :: soil_category       (:) ! soil texture type (integer index)
  integer             , allocatable :: slope_category      (:) ! slope category (integer index)
  real(kind=kind_phys), allocatable :: soil_interface_depth(:) ! soil layer-bottom depth from surface [m]
  integer             , allocatable :: ice_flag            (:) ! ice flag (1->ice)
  integer             , allocatable :: surface_type        (:) ! surface type flag 1->soil; 2->lake
  integer             , allocatable :: crop_type           (:) ! crop type category
  real(kind=kind_phys), allocatable :: temperature_soil_bot(:) ! soil bottom boundary condition temperature [K]

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
  integer :: surface_evap_resistance    ! option for surface resistent to evaporation/sublimation
  integer :: glacier                    ! option for glacier treatment

end type noahmp_options_type

type :: noahmp_model_type

  integer                           :: i_location                  ! grid index
  integer                           :: j_location                  ! grid index (not used in ccpp)
  integer                           :: year_length                 ! number of days in the current year
  real(kind=kind_phys)              :: julian_day                  ! julian day of year [floating point]
  real(kind=kind_phys), allocatable :: latitude                (:) ! latitude [radians]
  real(kind=kind_phys), allocatable :: cosine_zenith           (:) ! cosine solar zenith angle [-1,1]
  real(kind=kind_phys), allocatable :: forcing_height          (:) ! forcing height [m]
  real(kind=kind_phys), allocatable :: vegetation_frac         (:) ! vegetation fraction [0.0-1.0]
  real(kind=kind_phys), allocatable :: max_vegetation_frac     (:) ! annual maximum vegetation fraction [0.0-1.0]
  real(kind=kind_phys), allocatable :: snow_levels             (:) ! active snow levels [-]
  real(kind=kind_phys), allocatable :: interface_depth       (:,:) ! layer-bottom depth from snow surf [m]
  real(kind=kind_phys), allocatable :: snow_soil_thickness   (:,:) ! thickness of each snow/soil level [m]
  real(kind=kind_phys), allocatable :: leaf_area_index         (:) ! leaf area index [-]
  real(kind=kind_phys), allocatable :: stem_area_index         (:) ! stem area index [-]
  real(kind=kind_phys), allocatable :: growing_deg_days        (:) ! growing degree days [-]
  integer             , allocatable :: plant_growth_stage      (:) ! plant growing stage [-]
  real(kind=kind_phys), allocatable :: cm_noahmp               (:) ! grid momentum drag coefficient [m/s]
  real(kind=kind_phys), allocatable :: ch_noahmp               (:) ! grid heat exchange coefficient [m/s]
  real(kind=kind_phys), allocatable :: ch_vegetated            (:) ! vegetated heat exchange coefficient [m/s]
  real(kind=kind_phys), allocatable :: ch_bare_ground          (:) ! bare-ground heat exchange coefficient [m/s]
  real(kind=kind_phys), allocatable :: ch_leaf                 (:) ! leaf exchange coefficient [m/s]
  real(kind=kind_phys), allocatable :: ch_below_canopy         (:) ! below-canopy exchange coefficient [m/s]
  real(kind=kind_phys), allocatable :: ch_vegetated_2m         (:) ! 2-m vegetated  heat exchange coefficient [m/s]
  real(kind=kind_phys), allocatable :: ch_bare_ground_2m       (:) ! 2-m bare-ground heat exchange coefficient [m/s]
  real(kind=kind_phys), allocatable :: rs_sunlit               (:) ! sunlit leaf stomatal resistance [s/m]
  real(kind=kind_phys), allocatable :: rs_shaded               (:) ! shaded leaf stomatal resistance [s/m]
  real(kind=kind_phys), allocatable :: leaf_air_resistance     (:) ! leaf boundary layer resistance [s/m]

end type noahmp_model_type

type :: noahmp_forcing_type

  real(kind=kind_phys), allocatable :: air_pressure_forcing    (:) ! forcing air pressure [Pa]
  real(kind=kind_phys), allocatable :: precip_convective       (:) ! convective precipitation [mm/s]
  real(kind=kind_phys), allocatable :: precip_non_convective   (:) ! non-convective precipitation [mm/s]
  real(kind=kind_phys), allocatable :: precip_snow             (:) ! snow precipitation [mm/s]
  real(kind=kind_phys), allocatable :: precip_graupel          (:) ! graupel precipitation [mm/s]
  real(kind=kind_phys), allocatable :: precip_hail             (:) ! hail precipitation [mm/s]
  real(kind=kind_phys), allocatable :: snowfall                (:) ! land model partitioned snowfall [mm/s]
  real(kind=kind_phys), allocatable :: rainfall                (:) ! land model partitioned rainfall [mm/s]

end type noahmp_forcing_type

type :: noahmp_diag_type

  real(kind=kind_phys), allocatable :: z0_total                (:) ! weighted z0 sent to coupled model [m]
  real(kind=kind_phys), allocatable :: albedo_total            (:) ! total surface albedo [-]
  real(kind=kind_phys), allocatable :: albedo_direct         (:,:) ! direct vis/nir albedo [-]
  real(kind=kind_phys), allocatable :: albedo_diffuse        (:,:) ! diffuse vis/nir albedo [-]
  real(kind=kind_phys), allocatable :: albedo_direct_snow    (:,:) ! direct vis/nir snow albedo [-]
  real(kind=kind_phys), allocatable :: albedo_diffuse_snow   (:,:) ! diffuse vis/nir snow albedo [-]
  real(kind=kind_phys), allocatable :: emissivity_total        (:) ! grid emissivity [-]
  real(kind=kind_phys), allocatable :: canopy_gap_fraction     (:) ! between canopy gap fraction [-]
  real(kind=kind_phys), allocatable :: incanopy_gap_fraction   (:) ! within canopy gap fraction for beam [-]
  real(kind=kind_phys), allocatable :: precip_frozen_frac      (:) ! precipitation snow fraction [-]
  real(kind=kind_phys), allocatable :: snow_cover_fraction     (:) ! snow cover fraction on the ground [-]
  real(kind=kind_phys), allocatable :: vegetation_fraction     (:) ! vegetation fraction [0.0-1.0]
  real(kind=kind_phys), allocatable :: canopy_wet_fraction     (:) ! wetted or snowed fraction of canopy [-]
  real(kind=kind_phys), allocatable :: canopy_water            (:) ! canopy-intercepted water [mm]
  real(kind=kind_phys), allocatable :: depth_water_table       (:) ! depth to water table [m]
  real(kind=kind_phys), allocatable :: lai_sunlit              (:) ! sunlit leaf area index [m2/m2]
  real(kind=kind_phys), allocatable :: lai_shaded              (:) ! shaded leaf area index [m2/m2]
  real(kind=kind_phys), allocatable :: snow_ice_frac_old     (:,:) ! snow ice fraction at last timestep [-]
  real(kind=kind_phys), allocatable :: snow_albedo_old         (:) ! snow albedo at last time step [-]
  real(kind=kind_phys), allocatable :: evaporation_potential   (:) ! potential evaporation [mm/s]
  real(kind=kind_phys), allocatable :: soil_moisture_total     (:) ! total soil moisture in all levels [mm]
  real(kind=kind_phys), allocatable :: temperature_veg_2m      (:) ! vegetated 2-m air temperature [K]
  real(kind=kind_phys), allocatable :: temperature_bare_2m     (:) ! bare ground 2-m air temperature [K]
  real(kind=kind_phys), allocatable :: temperature_2m          (:) ! composite 2-m air temperature [K]
  real(kind=kind_phys), allocatable :: spec_humidity_veg_2m    (:) ! vegetated 2-m air specific humidity [K]
  real(kind=kind_phys), allocatable :: spec_humidity_bare_2m   (:) ! bare ground 2-m air specfic humidity [K]
  real(kind=kind_phys), allocatable :: spec_humidity_2m        (:) ! composite 2-m air specfic humidity [K]
  real(kind=kind_phys), allocatable :: spec_humidity_surface   (:) ! surface specific humidty [kg/kg]

end type noahmp_diag_type

type :: noahmp_state_type

  real(kind=kind_phys), allocatable :: temperature_soil      (:,:) ! soil temperature [K]
  real(kind=kind_phys), allocatable :: temperature_snow      (:,:) ! snow temperature [K]
  real(kind=kind_phys), allocatable :: temperature_canopy_air  (:) ! canopy air temperature [K]
  real(kind=kind_phys), allocatable :: temperature_radiative   (:) ! surface radiative temperature [K]
  real(kind=kind_phys), allocatable :: temperature_leaf        (:) ! leaf temperature [K]
  real(kind=kind_phys), allocatable :: temperature_ground      (:) ! grid ground surface temperature [K]
  real(kind=kind_phys), allocatable :: temperature_bare_grd    (:) ! bare ground surface temperature [K]
  real(kind=kind_phys), allocatable :: temperature_veg_grd     (:) ! below_canopy ground surface temperature [K]
  real(kind=kind_phys), allocatable :: vapor_pres_canopy_air   (:) ! canopy air vapor pressure [Pa]
  real(kind=kind_phys), allocatable :: soil_liquid_vol       (:,:) ! volumetric liquid soil moisture [m3/m3]
  real(kind=kind_phys), allocatable :: soil_moisture_vol     (:,:) ! volumetric soil moisture (ice + liq.) [m3/m3]
  real(kind=kind_phys), allocatable :: snow_water_equiv        (:) ! snow water equivalent [kg/m2]
  real(kind=kind_phys), allocatable :: snow_level_ice        (:,:) ! snow level ice [mm]
  real(kind=kind_phys), allocatable :: snow_level_liquid     (:,:) ! snow level liquid [mm]
  real(kind=kind_phys), allocatable :: canopy_liquid           (:) ! canopy-intercepted liquid [mm]
  real(kind=kind_phys), allocatable :: canopy_ice              (:) ! canopy-intercepted ice [mm]
  real(kind=kind_phys), allocatable :: aquifer_water           (:) ! water storage in aquifer [mm]
  real(kind=kind_phys), allocatable :: saturated_water         (:) ! water in aquifer+saturated soil [mm]
  real(kind=kind_phys), allocatable :: lake_water              (:) ! lake water storage [mm]
  real(kind=kind_phys), allocatable :: soil_moisture_wtd       (:) ! (opt_run=5) soil water content between bottom of the soil and water table [m3/m3]
  real(kind=kind_phys), allocatable :: eq_soil_water_vol     (:,:) ! (opt_run=5) equilibrium soil water content [m3/m3]
  real(kind=kind_phys), allocatable :: leaf_carbon             (:) ! leaf mass [g/m2]
  real(kind=kind_phys), allocatable :: root_carbon             (:) ! mass of fine roots [g/m2]
  real(kind=kind_phys), allocatable :: stem_carbon             (:) ! stem mass [g/m2]
  real(kind=kind_phys), allocatable :: wood_carbon             (:) ! mass of wood (incl. woody roots) [g/m2]
  real(kind=kind_phys), allocatable :: soil_carbon_stable      (:) ! stable soil carbon [g/m2]
  real(kind=kind_phys), allocatable :: soil_carbon_fast        (:) ! short-lived soil carbon [g/m2]
  real(kind=kind_phys), allocatable :: grain_carbon            (:) ! grain mass [g/m2]
  real(kind=kind_phys), allocatable :: foliage_nitrogen        (:) ! foliage nitrogen [%] [1-saturated]
  real(kind=kind_phys), allocatable :: snow_water_equiv_old    (:) ! snow water equivalent at last time step [kg/m2]
  real(kind=kind_phys), allocatable :: snow_depth              (:) ! snow depth [m]
  real(kind=kind_phys), allocatable :: snow_age                (:) ! non-dimensional snow age [-]

end type noahmp_state_type

type :: noahmp_flux_type

  real(kind=kind_phys), allocatable :: sw_absorbed_total       (:) ! total absorbed solar radiation [W/m2]
  real(kind=kind_phys), allocatable :: sw_reflected_total      (:) ! total reflected solar radiation [W/m2]
  real(kind=kind_phys), allocatable :: lw_absorbed_total       (:) ! total net lw rad [W/m2]  [+ to atm]
  real(kind=kind_phys), allocatable :: sensible_heat_total     (:) ! total sensible heat [W/m2] [+ to atm]
  real(kind=kind_phys), allocatable :: transpiration_heat      (:) ! transpiration heat flux [W/m2] [+ to atm]
  real(kind=kind_phys), allocatable :: latent_heat_canopy      (:) ! canopy evaporation heat flux [W/m2] [+ to atm]
  real(kind=kind_phys), allocatable :: latent_heat_ground      (:) ! ground evaporation heat flux [W/m2] [+ to atm]
  real(kind=kind_phys), allocatable :: latent_heat_total       (:) ! total heat flux [W/m2] [+ to atm]
  real(kind=kind_phys), allocatable :: ground_heat_total       (:) ! ground heat flux [W/m2]   [+ to soil]
  real(kind=kind_phys), allocatable :: precip_adv_heat_total   (:) ! precipitation advected heat - total [W/m2)
  real(kind=kind_phys), allocatable :: sw_absorbed_veg         (:) ! solar radiation absorbed by vegetation [W/m2]
  real(kind=kind_phys), allocatable :: sw_absorbed_ground      (:) ! solar radiation absorbed by ground [W/m2]
  real(kind=kind_phys), allocatable :: lw_absorbed_grd_veg     (:) ! below-canopy ground absorbed longwave radiation [W/m2]
  real(kind=kind_phys), allocatable :: lw_absorbed_leaf        (:) ! leaf absorbed longwave radiation [W/m2]
  real(kind=kind_phys), allocatable :: lw_absorbed_grd_bare    (:) ! bare ground net longwave radiation [W/m2]
  real(kind=kind_phys), allocatable :: sensible_heat_grd_veg   (:) ! below-canopy ground sensible heat flux [W/m2]
  real(kind=kind_phys), allocatable :: sensible_heat_leaf      (:) ! leaf-to-canopy sensible heat flux [W/m2]
  real(kind=kind_phys), allocatable :: sensible_heat_grd_bar   (:) ! bare ground sensible heat flux [W/m2]
  real(kind=kind_phys), allocatable :: latent_heat_trans       (:) ! transpiration [W/m2]
  real(kind=kind_phys), allocatable :: latent_heat_leaf        (:) ! leaf evaporation [W/m2]
  real(kind=kind_phys), allocatable :: latent_heat_grd_veg     (:) ! below-canopy ground evaporation heat flux [W/m2]
  real(kind=kind_phys), allocatable :: latent_heat_grd_bare    (:) ! bare ground evaporation heat flux [W/m2]
  real(kind=kind_phys), allocatable :: snow_sublimation        (:) ! snow sublimation [W/m2]
  real(kind=kind_phys), allocatable :: ground_heat_veg         (:) ! below-canopy ground heat flux [W/m2]
  real(kind=kind_phys), allocatable :: ground_heat_bare        (:) ! bare ground heat flux [W/m2]
  real(kind=kind_phys), allocatable :: precip_adv_heat_veg     (:) ! precipitation advected heat - vegetation net [W/m2]
  real(kind=kind_phys), allocatable :: precip_adv_heat_grd_v   (:) ! precipitation advected heat - below-canopy net [W/m2]
  real(kind=kind_phys), allocatable :: precip_adv_heat_grd_b   (:) ! precipitation advected heat - bare ground net [W/m2]
  real(kind=kind_phys), allocatable :: transpiration           (:) ! transpiration [mm/s]
  real(kind=kind_phys), allocatable :: evaporation_canopy      (:) ! canopy evaporation [mm/s]
  real(kind=kind_phys), allocatable :: evaporation_soil        (:) ! soil surface evaporation [mm/s]
  real(kind=kind_phys), allocatable :: runoff_surface          (:) ! surface runoff [mm/s] 
  real(kind=kind_phys), allocatable :: runoff_baseflow         (:) ! baseflow runoff [mm/s]
  real(kind=kind_phys), allocatable :: snowmelt_out            (:) ! snowmelt out bottom of pack [mm/s]
  real(kind=kind_phys), allocatable :: snowmelt_shallow        (:) ! shallow snow melt [mm/timestep]
  real(kind=kind_phys), allocatable :: snowmelt_shallow_1      (:) ! additional shallow snow melt [mm/timestep]
  real(kind=kind_phys), allocatable :: snowmelt_shallow_2      (:) ! additional shallow snow melt [mm/timestep]
  real(kind=kind_phys), allocatable :: deep_recharge           (:) ! (opt_run=5) recharge to or from the water table when deep [m]
  real(kind=kind_phys), allocatable :: recharge                (:) ! (opt_run=5) recharge to or from the water table when shallow [m]
  real(kind=kind_phys), allocatable :: par_absorbed            (:) ! absorbed photosynthesis active radiation [W/m2]
  real(kind=kind_phys), allocatable :: photosynthesis          (:) ! total photosynthesis [umol CO2/m2/s] [+ out]
  real(kind=kind_phys), allocatable :: net_eco_exchange        (:) ! net ecosystem exchange [g/m2/s CO2]
  real(kind=kind_phys), allocatable :: global_prim_prod        (:) ! global primary production [g/m2/s C]
  real(kind=kind_phys), allocatable :: net_prim_prod           (:) ! net primary productivity [g/m2/s C]

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

    allocate(this%static%vegetation_category    (vector_length)            ) ! vegetation type (integer index)
    allocate(this%static%soil_category          (vector_length)            ) ! soil texture type (integer index)
    allocate(this%static%slope_category         (vector_length)            ) ! slope category (integer index)
    allocate(this%static%soil_interface_depth   (vector_length)            ) ! soil layer-bottom depth from surface [m]
    allocate(this%static%ice_flag               (vector_length)            ) ! ice flag (1->ice)
    allocate(this%static%surface_type           (vector_length)            ) ! surface type flag 1->soil; 2->lake
    allocate(this%static%crop_type              (vector_length)            ) ! crop type category
    allocate(this%static%temperature_soil_bot   (vector_length)            ) ! soil bottom boundary condition temperature [K]

    allocate(this%model%latitude                (vector_length)            ) ! latitude [radians]
    allocate(this%model%cosine_zenith           (vector_length)            ) ! cosine solar zenith angle [-1,1]
    allocate(this%model%forcing_height          (vector_length)            ) ! forcing height [m]
    allocate(this%model%vegetation_frac         (vector_length)            ) ! vegetation fraction [0.0-1.0]
    allocate(this%model%max_vegetation_frac     (vector_length)            ) ! annual maximum vegetation fraction [0.0-1.0]
    allocate(this%model%snow_levels             (vector_length)            ) ! active snow levels [-]
    allocate(this%model%interface_depth         (vector_length,snow_index:soil_levels)) ! layer-bottom depth from snow surf [m]
    allocate(this%model%snow_soil_thickness     (vector_length,snow_index:soil_levels)) ! thickness of each snow/soil level [m]
    allocate(this%model%leaf_area_index         (vector_length)            ) ! leaf area index [-]
    allocate(this%model%stem_area_index         (vector_length)            ) ! stem area index [-]
    allocate(this%model%growing_deg_days        (vector_length)            ) ! growing degree days [-]
    allocate(this%model%plant_growth_stage      (vector_length)            ) ! plant growing stage [-]
    allocate(this%model%cm_noahmp               (vector_length)            ) ! grid momentum drag coefficient [m/s]
    allocate(this%model%ch_noahmp               (vector_length)            ) ! grid heat exchange coefficient [m/s]
    allocate(this%model%ch_vegetated            (vector_length)            ) ! vegetated heat exchange coefficient [m/s]
    allocate(this%model%ch_bare_ground          (vector_length)            ) ! bare-ground heat exchange coefficient [m/s]
    allocate(this%model%ch_leaf                 (vector_length)            ) ! leaf exchange coefficient [m/s]
    allocate(this%model%ch_below_canopy         (vector_length)            ) ! below-canopy exchange coefficient [m/s]
    allocate(this%model%ch_vegetated_2m         (vector_length)            ) ! 2-m vegetated  heat exchange coefficient [m/s]
    allocate(this%model%ch_bare_ground_2m       (vector_length)            ) ! 2-m bare-ground heat exchange coefficient [m/s]
    allocate(this%model%rs_sunlit               (vector_length)            ) ! sunlit leaf stomatal resistance [s/m]
    allocate(this%model%rs_shaded               (vector_length)            ) ! shaded leaf stomatal resistance [s/m]
    allocate(this%model%leaf_air_resistance     (vector_length)            ) ! leaf boundary layer resistance [s/m]

    allocate(this%forcing%air_pressure_forcing  (vector_length)            ) ! forcing air pressure [Pa]
    allocate(this%forcing%precip_convective     (vector_length)            ) ! convective precipitation [mm/s]
    allocate(this%forcing%precip_non_convective (vector_length)            ) ! non-convective precipitation [mm/s]
    allocate(this%forcing%precip_snow           (vector_length)            ) ! snow precipitation [mm/s]
    allocate(this%forcing%precip_graupel        (vector_length)            ) ! graupel precipitation [mm/s]
    allocate(this%forcing%precip_hail           (vector_length)            ) ! hail precipitation [mm/s]
    allocate(this%forcing%snowfall              (vector_length)            ) ! land model partitioned snowfall [mm/s]
    allocate(this%forcing%rainfall              (vector_length)            ) ! land model partitioned rainfall [mm/s]

    allocate(this%diag%z0_total                 (vector_length)            ) ! weighted z0 sent to coupled model [m]
    allocate(this%diag%albedo_total             (vector_length)            ) ! total surface albedo [-]
    allocate(this%diag%albedo_direct            (vector_length,2)          ) ! direct vis/nir albedo [-]
    allocate(this%diag%albedo_diffuse           (vector_length,2)          ) ! diffuse vis/nir albedo [-]
    allocate(this%diag%albedo_direct_snow       (vector_length,2)          ) ! direct vis/nir snow albedo [-]
    allocate(this%diag%albedo_diffuse_snow      (vector_length,2)          ) ! diffuse vis/nir snow albedo [-]
    allocate(this%diag%emissivity_total         (vector_length)            ) ! grid emissivity [-]
    allocate(this%diag%canopy_gap_fraction      (vector_length)            ) ! between canopy gap fraction [-]
    allocate(this%diag%incanopy_gap_fraction    (vector_length)            ) ! within canopy gap fraction for beam [-]
    allocate(this%diag%precip_frozen_frac       (vector_length)            ) ! precipitation snow fraction [-]
    allocate(this%diag%snow_cover_fraction      (vector_length)            ) ! snow cover fraction on the ground [-]
    allocate(this%diag%vegetation_fraction      (vector_length)            ) ! vegetation fraction [0.0-1.0]
    allocate(this%diag%canopy_wet_fraction      (vector_length)            ) ! wetted or snowed fraction of canopy [-]
    allocate(this%diag%canopy_water             (vector_length)            ) ! canopy-intercepted water [mm]
    allocate(this%diag%depth_water_table        (vector_length)            ) ! depth to water table [m]
    allocate(this%diag%lai_sunlit               (vector_length)            ) ! sunlit leaf area index [m2/m2]
    allocate(this%diag%lai_shaded               (vector_length)            ) ! shaded leaf area index [m2/m2]
    allocate(this%diag%snow_ice_frac_old        (vector_length,snow_index:0)) ! snow ice fraction at last timestep [-]
    allocate(this%diag%snow_albedo_old          (vector_length)            ) ! snow albedo at last time step [-]
    allocate(this%diag%evaporation_potential    (vector_length)            ) ! potential evaporation [mm/s]
    allocate(this%diag%soil_moisture_total      (vector_length)            ) ! total soil moisture in all levels [mm]
    allocate(this%diag%temperature_veg_2m       (vector_length)            ) ! vegetated 2-m air temperature [K]
    allocate(this%diag%temperature_bare_2m      (vector_length)            ) ! bare ground 2-m air temperature [K]
    allocate(this%diag%temperature_2m           (vector_length)            ) ! composite 2-m air temperature [K]
    allocate(this%diag%spec_humidity_veg_2m     (vector_length)            ) ! vegetated 2-m air specific humidity [K]
    allocate(this%diag%spec_humidity_bare_2m    (vector_length)            ) ! bare ground 2-m air specfic humidity [K]
    allocate(this%diag%spec_humidity_2m         (vector_length)            ) ! composite 2-m air specfic humidity [K]
    allocate(this%diag%spec_humidity_surface    (vector_length)            ) ! surface specific humidty [kg/kg]

    allocate(this%state%temperature_soil        (vector_length,soil_levels)) ! soil temperature [K]
    allocate(this%state%temperature_snow        (vector_length,snow_index:0)) ! snow temperature [K]
    allocate(this%state%temperature_canopy_air  (vector_length)            ) ! canopy air temperature [K]
    allocate(this%state%temperature_radiative   (vector_length)            ) ! surface radiative temperature [K]
    allocate(this%state%temperature_leaf        (vector_length)            ) ! leaf temperature [K]
    allocate(this%state%temperature_ground      (vector_length)            ) ! grid ground surface temperature [K]
    allocate(this%state%temperature_bare_grd    (vector_length)            ) ! bare ground surface temperature [K]
    allocate(this%state%temperature_veg_grd     (vector_length)            ) ! below_canopy ground surface temperature [K]
    allocate(this%state%vapor_pres_canopy_air   (vector_length)            ) ! canopy air vapor pressure [Pa]
    allocate(this%state%soil_liquid_vol         (vector_length,soil_levels)) ! volumetric liquid soil moisture [m3/m3]
    allocate(this%state%soil_moisture_vol       (vector_length,soil_levels)) ! volumetric soil moisture (ice + liq.) [m3/m3]
    allocate(this%state%snow_water_equiv        (vector_length)            ) ! snow water equivalent [kg/m2]
    allocate(this%state%snow_level_ice          (vector_length,snow_index:0)) ! snow level ice [mm]
    allocate(this%state%snow_level_liquid       (vector_length,snow_index:0)) ! snow level liquid [mm]
    allocate(this%state%canopy_liquid           (vector_length)            ) ! canopy-intercepted liquid [mm]
    allocate(this%state%canopy_ice              (vector_length)            ) ! canopy-intercepted ice [mm]
    allocate(this%state%aquifer_water           (vector_length)            ) ! water storage in aquifer [mm]
    allocate(this%state%saturated_water         (vector_length)            ) ! water in aquifer+saturated soil [mm]
    allocate(this%state%lake_water              (vector_length)            ) ! lake water storage [mm]
    allocate(this%state%soil_moisture_wtd       (vector_length)            ) ! (opt_run=5) soil water content between bottom of the soil and water table [m3/m3]
    allocate(this%state%eq_soil_water_vol       (vector_length,soil_levels)) ! (opt_run=5) equilibrium soil water content [m3/m3]
    allocate(this%state%leaf_carbon             (vector_length)            ) ! leaf mass [g/m2]
    allocate(this%state%root_carbon             (vector_length)            ) ! mass of fine roots [g/m2]
    allocate(this%state%stem_carbon             (vector_length)            ) ! stem mass [g/m2]
    allocate(this%state%wood_carbon             (vector_length)            ) ! mass of wood (incl. woody roots) [g/m2]
    allocate(this%state%soil_carbon_stable      (vector_length)            ) ! stable soil carbon [g/m2]
    allocate(this%state%soil_carbon_fast        (vector_length)            ) ! short-lived soil carbon [g/m2]
    allocate(this%state%grain_carbon            (vector_length)            ) ! grain mass [g/m2]
    allocate(this%state%foliage_nitrogen        (vector_length)            ) ! foliage nitrogen [%] [1-saturated]
    allocate(this%state%snow_water_equiv_old    (vector_length)            ) ! snow water equivalent at last time step [kg/m2]
    allocate(this%state%snow_depth              (vector_length)            ) ! snow depth [m]
    allocate(this%state%snow_age                (vector_length)            ) ! non-dimensional snow age [-]

    allocate(this%flux%sw_absorbed_total        (vector_length)            ) ! total absorbed solar radiation [W/m2]
    allocate(this%flux%sw_reflected_total       (vector_length)            ) ! total reflected solar radiation [W/m2]
    allocate(this%flux%lw_absorbed_total        (vector_length)            ) ! total net lw rad [W/m2]  [+ to atm]
    allocate(this%flux%sensible_heat_total      (vector_length)            ) ! total sensible heat [W/m2] [+ to atm]
    allocate(this%flux%transpiration_heat       (vector_length)            ) ! transpiration heat flux [W/m2] [+ to atm]
    allocate(this%flux%latent_heat_canopy       (vector_length)            ) ! canopy evaporation heat flux [W/m2] [+ to atm]
    allocate(this%flux%latent_heat_ground       (vector_length)            ) ! ground evaporation heat flux [W/m2] [+ to atm]
    allocate(this%flux%latent_heat_total        (vector_length)            ) ! total latent heat flux [W/m2] [+ to atm]
    allocate(this%flux%ground_heat_total        (vector_length)            ) ! ground heat flux [W/m2]   [+ to soil]
    allocate(this%flux%precip_adv_heat_total    (vector_length)            ) ! precipitation advected heat - total [W/m2)
    allocate(this%flux%sw_absorbed_veg          (vector_length)            ) ! solar radiation absorbed by vegetation [W/m2]
    allocate(this%flux%sw_absorbed_ground       (vector_length)            ) ! solar radiation absorbed by ground [W/m2]
    allocate(this%flux%lw_absorbed_grd_veg      (vector_length)            ) ! below-canopy ground absorbed longwave radiation [W/m2]
    allocate(this%flux%lw_absorbed_leaf         (vector_length)            ) ! leaf absorbed longwave radiation [W/m2]
    allocate(this%flux%lw_absorbed_grd_bare     (vector_length)            ) ! bare ground net longwave radiation [W/m2]
    allocate(this%flux%sensible_heat_grd_veg    (vector_length)            ) ! below-canopy ground sensible heat flux [W/m2]
    allocate(this%flux%sensible_heat_leaf       (vector_length)            ) ! leaf-to-canopy sensible heat flux [W/m2]
    allocate(this%flux%sensible_heat_grd_bar    (vector_length)            ) ! bare ground sensible heat flux [W/m2]
    allocate(this%flux%latent_heat_trans        (vector_length)            ) ! transpiration [W/m2]
    allocate(this%flux%latent_heat_leaf         (vector_length)            ) ! leaf evaporation [W/m2]
    allocate(this%flux%latent_heat_grd_veg      (vector_length)            ) ! below-canopy ground evaporation heat flux [W/m2]
    allocate(this%flux%latent_heat_grd_bare     (vector_length)            ) ! bare ground evaporation heat flux [W/m2]
    allocate(this%flux%snow_sublimation         (vector_length)            ) ! snow sublimation [W/m2]
    allocate(this%flux%ground_heat_veg          (vector_length)            ) ! below-canopy ground heat flux [W/m2]
    allocate(this%flux%ground_heat_bare         (vector_length)            ) ! bare ground heat flux [W/m2]
    allocate(this%flux%precip_adv_heat_veg      (vector_length)            ) ! precipitation advected heat - vegetation net [W/m2]
    allocate(this%flux%precip_adv_heat_grd_v    (vector_length)            ) ! precipitation advected heat - below-canopy net [W/m2]
    allocate(this%flux%precip_adv_heat_grd_b    (vector_length)            ) ! precipitation advected heat - bare ground net [W/m2]
    allocate(this%flux%transpiration            (vector_length)            ) ! transpiration [mm/s]
    allocate(this%flux%evaporation_canopy       (vector_length)            ) ! canopy evaporation [mm/s]
    allocate(this%flux%evaporation_soil         (vector_length)            ) ! soil surface evaporation [mm/s]
    allocate(this%flux%runoff_surface           (vector_length)            ) ! surface runoff [mm/s] 
    allocate(this%flux%runoff_baseflow          (vector_length)            ) ! baseflow runoff [mm/s]
    allocate(this%flux%snowmelt_out             (vector_length)            ) ! snowmelt out bottom of pack [mm/s]
    allocate(this%flux%snowmelt_shallow         (vector_length)            ) ! shallow snow melt [mm/timestep]
    allocate(this%flux%snowmelt_shallow_1       (vector_length)            ) ! additional shallow snow melt [mm/timestep]
    allocate(this%flux%snowmelt_shallow_2       (vector_length)            ) ! additional shallow snow melt [mm/timestep]
    allocate(this%flux%deep_recharge            (vector_length)            ) ! (opt_run=5) recharge to or from the water table when deep [m]
    allocate(this%flux%recharge                 (vector_length)            ) ! (opt_run=5) recharge to or from the water table when shallow [m]
    allocate(this%flux%par_absorbed             (vector_length)            ) ! absorbed photosynthesis active radiation [W/m2]
    allocate(this%flux%photosynthesis           (vector_length)            ) ! total photosynthesis [umol CO2/m2/s] [+ out]
    allocate(this%flux%net_eco_exchange         (vector_length)            ) ! net ecosystem exchange [g/m2/s CO2]
    allocate(this%flux%global_prim_prod         (vector_length)            ) ! global primary production [g/m2/s C]
    allocate(this%flux%net_prim_prod            (vector_length)            ) ! net primary productivity [g/m2/s C]

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(noahmp_type) :: this


  end subroutine InitDefault

  subroutine TransferNamelist(this, namelist)
    
  class(noahmp_type)     :: this
  type(namelist_type)  :: namelist
  
  this%static%timestep                    = namelist%timestep_seconds
  this%model%forcing_height               = namelist%forcing_height
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
  
    this%state%temperature_leaf      (iloc) = this%state%temperature_radiative(iloc)
    this%state%temperature_ground    (iloc) = this%state%temperature_radiative(iloc)
    this%state%temperature_canopy_air(iloc) = this%state%temperature_radiative(iloc)

    if (this%state%snow_depth(iloc) > 0.01 .and. this%state%temperature_radiative(iloc) > 273.15 ) then
      this%state%temperature_leaf      (iloc) = 273.15
      this%state%temperature_ground    (iloc) = 273.15
      this%state%temperature_canopy_air(iloc) = 273.15
    end if
  
    this%state%canopy_ice   (iloc) = 0.0
    this%state%canopy_liquid(iloc) = this%diag%canopy_water(iloc)

    this%state%vapor_pres_canopy_air(iloc) = 2000.0

    this%model%cm_noahmp            (iloc) = 0.0
    this%model%ch_noahmp            (iloc) = 0.0
    this%diag%canopy_wet_fraction   (iloc) = 0.0
    this%state%snow_water_equiv_old (iloc) = this%state%snow_water_equiv(iloc)
    this%diag%snow_albedo_old       (iloc) = 0.65
    this%forcing%snowfall           (iloc) = 0.0

    this%state%lake_water(iloc) = 0.0
    this%state%snow_age  (iloc) = 0.0

    this%state%aquifer_water      (iloc) = 4900.0                ! this assumes water table is at 2.5m
    this%state%saturated_water    (iloc) = this%state%aquifer_water(iloc)
    this%diag%depth_water_table   (iloc) = (25.0 + 2.0) - this%state%aquifer_water(iloc) / 1000.0 /0.2

    if ((this%static%vegetation_category(iloc) == isbarren_table) .or. &
        (this%static%vegetation_category(iloc) == isice_table   ) .or. &
        (this%static%vegetation_category(iloc) == isurban_table ) .or. &
        (this%static%vegetation_category(iloc) == iswater_table )) then

      this%model%leaf_area_index  (iloc) = 0.0
      this%model%stem_area_index  (iloc) = 0.0

      this%state%leaf_carbon(iloc) = 0.0
      this%state%stem_carbon(iloc) = 0.0
      this%state%root_carbon(iloc) = 0.0

      this%state%wood_carbon       (iloc) = 0.0       
      this%state%soil_carbon_stable(iloc) = 0.0      
      this%state%soil_carbon_fast  (iloc) = 0.0     

    else

      this%model%leaf_area_index  (iloc) = max(laim_table(this%static%vegetation_category(iloc), current_mm),0.05)
      this%model%stem_area_index  (iloc) = max(this%model%leaf_area_index(iloc)*0.1,0.05)

      masslai                      = 1000.0 / max(sla_table(this%static%vegetation_category(iloc)),1.0)
      this%state%leaf_carbon(iloc) = this%model%leaf_area_index(iloc) * masslai
      masssai                      = 1000.0 / 3.0
      this%state%stem_carbon(iloc) = this%model%stem_area_index(iloc) * masssai

      this%state%root_carbon       (iloc) = 500.0      
      this%state%wood_carbon       (iloc) = 500.0       
      this%state%soil_carbon_stable(iloc) = 1000.0      
      this%state%soil_carbon_fast  (iloc) = 1000.0     

    end if

    if (this%static%vegetation_category(iloc)  == isice_table )  then
      do ilevel = 1, namelist%num_soil_levels
        this%state%temperature_soil(iloc,ilevel) = &
           min(this%state%temperature_soil(iloc,ilevel),min(this%static%temperature_soil_bot(iloc),263.15))
      end do
      this%state%soil_liquid_vol  (iloc,:) = 0.0
      this%state%soil_moisture_vol(iloc,:) = 1.0
    end if

    snow_depth_meters = this%state%snow_depth(iloc) / 1000.0  ! snow depth in meters

    if (this%state%snow_water_equiv(iloc) /= 0.0 .and. snow_depth_meters == 0.0 ) then
      snow_depth_meters = this%state%snow_water_equiv(iloc) / 1000.0 * 10.0   ! assume 10/1 ratio
    endif

    if (this%static%vegetation_category(iloc) == isice_table) then  ! land ice in MODIS/IGBP
      if (this%state%snow_water_equiv(iloc) < 0.1) then
        this%state%snow_water_equiv(iloc) = 0.1
        snow_depth_meters = this%state%snow_water_equiv(iloc) / 1000.0 * 10.0 
      end if
    end if

    this%model%snow_soil_thickness = 0.0
    if (snow_depth_meters < 0.025 ) then
      this%model%snow_levels(iloc)              = 0.0
      this%model%snow_soil_thickness(iloc,-2:0) = 0.0
    elseif (snow_depth_meters >= 0.025 .and. snow_depth_meters <= 0.05 ) then
      this%model%snow_levels(iloc)              = -1.0
      this%model%snow_soil_thickness(iloc,0)    = snow_depth_meters
    elseif (snow_depth_meters > 0.05 .and. snow_depth_meters <= 0.10 ) then
      this%model%snow_levels(iloc)              = -2.0
      this%model%snow_soil_thickness(iloc,-1)   = 0.5*snow_depth_meters
      this%model%snow_soil_thickness(iloc,0)    = 0.5*snow_depth_meters
    elseif (snow_depth_meters > 0.10 .and. snow_depth_meters <= 0.25 ) then
      this%model%snow_levels(iloc)                   = -2.0
      this%model%snow_soil_thickness(iloc,-1)   = 0.05
      this%model%snow_soil_thickness(iloc,0)    = snow_depth_meters - 0.05
    elseif (snow_depth_meters > 0.25 .and. snow_depth_meters <= 0.45 ) then
      this%model%snow_levels(iloc)                   = -3.0
      this%model%snow_soil_thickness(iloc,-2)   = 0.05
      this%model%snow_soil_thickness(iloc,-1)   = 0.5*(snow_depth_meters-0.05)
      this%model%snow_soil_thickness(iloc,0)    = 0.5*(snow_depth_meters-0.05)
    elseif (snow_depth_meters > 0.45) then 
      this%model%snow_levels(iloc)                   = -3.0
      this%model%snow_soil_thickness(iloc,-2)   = 0.05
      this%model%snow_soil_thickness(iloc,-1)   = 0.20
      this%model%snow_soil_thickness(iloc,0)    = snow_depth_meters - 0.05 - 0.20
    else
      ! call mpp_error(FATAL, 'problem with the logic assigning snow layers.') 
    endif

! Now we have the snow_levels field
! snice + snliq + tsno allocation and compute them from what we have
             
    this%state%temperature_snow (iloc,-2:0) = 0.0
    this%state%snow_level_ice   (iloc,-2:0) = 0.0
    this%state%snow_level_liquid(iloc,-2:0) = 0.0
    this%model%interface_depth  (iloc,-2:namelist%num_soil_levels) = 0.0

    isnow = nint(this%model%snow_levels(iloc)) + 1    ! snow_levels <=0.0, interface_depth >= 0.0

    do ilevel = isnow , 0
      this%state%temperature_snow (iloc,ilevel) = this%state%temperature_ground(iloc)
      this%state%snow_level_liquid(iloc,ilevel) = 0.0
      this%state%snow_level_ice(iloc,ilevel)    = this%model%snow_soil_thickness(iloc,-2) / snow_depth_meters &
                                                   * this%state%snow_water_equiv(iloc)
    end do

    this%model%interface_depth(iloc,isnow) = -1.0*this%model%snow_soil_thickness(iloc,isnow)
    do ilevel = isnow+1, 0
      this%model%interface_depth(iloc,ilevel) = this%model%interface_depth(iloc,ilevel-1) - &
                                                 this%model%snow_soil_thickness(iloc,ilevel)
    end do
    do ilevel = 1, namelist%num_soil_levels
      this%model%snow_soil_thickness(iloc,ilevel) = namelist%soil_level_thickness(ilevel)
      this%model%interface_depth(iloc,ilevel)     = this%model%interface_depth(iloc,ilevel-1) - &
                                                     namelist%soil_level_thickness(ilevel)
    end do

! Should not need to initialize these
    
    this%state%eq_soil_water_vol(iloc,:) = 0.0
    this%state%soil_moisture_wtd(iloc)   = 0.0
    this%flux%deep_recharge(iloc)        = 0.0
    this%flux%recharge(iloc)             = 0.0

  end do ! iloc
  
  end subroutine InitStates

end module ufsLandNoahMPType
