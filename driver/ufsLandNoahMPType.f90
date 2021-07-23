module ufsLandNoahMPType

use NamelistRead
use machine , only : kind_phys

implicit none
save
private

type :: noahmp_static_type

  integer                 ::   im         ! horiz dimension and num of used pts         1
  integer                 ::   km         ! vertical soil layer dimension               1
  integer                 ::   itime      ! NOT USED
  integer                 ::   isot       ! sfc soil type data source zobler or statsgo
  integer                 ::   ivegsrc    ! sfc veg type data source umd or igbp
  real(kind=kind_phys)    ::   con_hvap   ! lat heat H2O cond [J/kg]; 
  real(kind=kind_phys)    ::   con_cp     ! spec heat air at v [J/kg/K] 
  real(kind=kind_phys)    ::   con_jcal   ! joules per calorie
  real(kind=kind_phys)    ::   rhoh2o     ! density of water [kg/m^3]
  real(kind=kind_phys)    ::   con_eps    ! Rd/Rv 
  real(kind=kind_phys)    ::   con_epsm1  ! Rd/Rv - 1
  real(kind=kind_phys)    ::   con_fvirt  ! Rv/Rd - 1
  real(kind=kind_phys)    ::   con_rd     ! gas constant air [J/kg/K]
  real(kind=kind_phys)    ::   con_hfus   ! lat heat H2O fusion  [J/kg]
  real(kind=kind_phys)    ::   delt       ! time interval [s]                    1
  integer                 ::   idveg      ! option for dynamic vegetation
  integer                 ::   iopt_crs   ! option for canopy stomatal resistance
  integer                 ::   iopt_btr   ! option for soil moisture factor for stomatal resistance
  integer                 ::   iopt_run   ! option for runoff and groundwater
  integer                 ::   iopt_sfc   ! option for surface layer drag coeff (ch & cm)
  integer                 ::   iopt_frz   ! option for supercooled liquid water (or ice fraction)
  integer                 ::   iopt_inf   ! option for frozen soil permeability
  integer                 ::   iopt_rad   ! option for radiation transfer
  integer                 ::   iopt_alb   ! option for ground snow surface albedo
  integer                 ::   iopt_snf   ! option for partitioning  precipitation into rainfall & snowfall
  integer                 ::   iopt_tbot  ! option for lower boundary condition of soil temperature
  integer                 ::   iopt_stc   ! option for snow/soil temperature time scheme (only layer 1)
  integer                 ::   iopt_rsf   ! options for surface resistent to evaporation/sublimation
  integer                 ::   iopt_gla   ! option for glacier treatment
  character(len=128)      ::   errmsg     ! error messaging added to ccpp
  integer                 ::   errflg     ! error messaging added to ccpp

end type noahmp_static_type

type :: noahmp_model_type

  integer                           :: iyrlen        ! year length
  real(kind=kind_phys)              :: julian        ! julian day of year
  real(kind=kind_phys), allocatable :: u1        (:) ! u-component of wind [m/s]                   im
  real(kind=kind_phys), allocatable :: v1        (:) ! u-component of wind [m/s]                   im
  integer             , allocatable :: soiltyp   (:) ! soil type (integer index)                   im
  integer             , allocatable :: vegtype   (:) ! vegetation type (integer index)             im
  real(kind=kind_phys), allocatable :: sigmaf    (:) ! areal fractional cover of green vegetation  im
  real(kind=kind_phys), allocatable :: emiss     (:) ! sfc lw emissivity ( fraction )              im
  real(kind=kind_phys), allocatable :: albdvis   (:) ! albedo - direct  visible ( fraction )       im
  real(kind=kind_phys), allocatable :: albdnir   (:) ! albedo - direct  NIR     ( fraction )       im
  real(kind=kind_phys), allocatable :: albivis   (:) ! albedo - diffuse visible ( fraction )       im
  real(kind=kind_phys), allocatable :: albinir   (:) ! albedo - diffuse NIR     ( fraction )       im
  real(kind=kind_phys), allocatable :: snet      (:) ! total sky sfc netsw flx into ground[W/m**2] im NOT USED
  real(kind=kind_phys), allocatable :: tg3       (:) ! deep soil temperature [K]                   im
  real(kind=kind_phys), allocatable :: cm        (:) ! surface exchange coeff for momentum [m/s]   im
  real(kind=kind_phys), allocatable :: ch        (:) ! surface exchange coeff heat & moisture[m/s] im
  real(kind=kind_phys), allocatable :: prsl1     (:) ! sfc layer 1 mean pressure [Pa]              im
  real(kind=kind_phys), allocatable :: prslki    (:) !                                             im
  real(kind=kind_phys), allocatable :: zf        (:) ! height of bottom layer [m]                  im
  logical             , allocatable :: dry       (:) ! = T if a point with any land                im
  integer             , allocatable :: slopetyp  (:) ! class of sfc slope (integer index)          im
  real(kind=kind_phys), allocatable :: shdmin    (:) ! min fractional coverage of green veg        im NOT USED
  real(kind=kind_phys), allocatable :: shdmax    (:) ! max fractnl cover of green veg (not used)   im
  real(kind=kind_phys), allocatable :: snoalb    (:) ! upper bound on max albedo over deep snow    im NOT USED
  real(kind=kind_phys), allocatable :: sfalb     (:) ! mean sfc diffused sw albedo (fractional)    im NOT USED
  logical             , allocatable :: flag_iter (:) !                                             im
  logical             , allocatable :: flag_guess(:) !                                             im
  real(kind=kind_phys), allocatable :: xlatin     (:) ! latitude
  real(kind=kind_phys), allocatable :: xcoszin    (:) ! cosine of zenith angle
  real(kind=kind_phys), allocatable :: rainn_mp   (:) ! microphysics non-convective precipitation [mm]
  real(kind=kind_phys), allocatable :: rainc_mp   (:) ! microphysics convective precipitation [mm]
  real(kind=kind_phys), allocatable :: snow_mp    (:) ! microphysics snow [mm]
  real(kind=kind_phys), allocatable :: graupel_mp (:) ! microphysics graupel [mm]
  real(kind=kind_phys), allocatable :: ice_mp     (:) ! microphysics ice/hail [mm]
  real(kind=kind_phys), allocatable :: weasd     (:) ! water equivalent accumulated snow depth(mm) im
  real(kind=kind_phys), allocatable :: snwdph    (:) ! snow depth (water equiv) over land          im
  real(kind=kind_phys), allocatable :: tskin     (:) ! ground surface skin temperature ( k )       im
  real(kind=kind_phys), allocatable :: srflag    (:) ! snow/rain flag for precipitation            im
  real(kind=kind_phys), allocatable :: canopy    (:) ! canopy moisture content (m)                 im
  real(kind=kind_phys), allocatable :: trans     (:) ! total plant transpiration (m/s)             im
  real(kind=kind_phys), allocatable :: tsurf     (:) ! surface skin temperature (after iteration)  im
  real(kind=kind_phys), allocatable :: zorl      (:) ! surface roughness                           im
  real(kind=kind_phys), allocatable :: snowxy    (:) ! actual no. of snow layers
  real(kind=kind_phys), allocatable :: tvxy      (:) ! vegetation leaf temperature [K]
  real(kind=kind_phys), allocatable :: tgxy      (:) ! bulk ground surface temperature [K]
  real(kind=kind_phys), allocatable :: canicexy  (:) ! canopy-intercepted ice [mm]
  real(kind=kind_phys), allocatable :: canliqxy  (:) ! canopy-intercepted liquid water [mm]
  real(kind=kind_phys), allocatable :: eahxy     (:) ! canopy air vapor pressure [pa]
  real(kind=kind_phys), allocatable :: tahxy     (:) ! canopy air temperature [K]
  real(kind=kind_phys), allocatable :: cmxy      (:) ! bulk momentum drag coefficient [m/s]
  real(kind=kind_phys), allocatable :: chxy      (:) ! bulk sensible heat exchange coefficient [m/s]
  real(kind=kind_phys), allocatable :: fwetxy    (:) ! wetted or snowed fraction of the canopy [-]
  real(kind=kind_phys), allocatable :: sneqvoxy  (:) ! snow mass at last time step[mm h2o]
  real(kind=kind_phys), allocatable :: alboldxy  (:) ! snow albedo at last time step [-]
  real(kind=kind_phys), allocatable :: qsnowxy   (:) ! snowfall on the ground [mm/s]
  real(kind=kind_phys), allocatable :: wslakexy  (:) ! lake water storage [mm]
  real(kind=kind_phys), allocatable :: zwtxy     (:) ! water table depth [m]
  real(kind=kind_phys), allocatable :: waxy      (:) ! water in the "aquifer" [mm]
  real(kind=kind_phys), allocatable :: wtxy      (:) ! groundwater storage [mm]
  real(kind=kind_phys), allocatable :: lfmassxy  (:) ! leaf mass [g/m2]
  real(kind=kind_phys), allocatable :: rtmassxy  (:) ! mass of fine roots [g/m2]
  real(kind=kind_phys), allocatable :: stmassxy  (:) ! stem mass [g/m2]
  real(kind=kind_phys), allocatable :: woodxy    (:) ! mass of wood (incl. woody roots) [g/m2]
  real(kind=kind_phys), allocatable :: stblcpxy  (:) ! stable carbon in deep soil [g/m2]
  real(kind=kind_phys), allocatable :: fastcpxy  (:) ! short-lived carbon, shallow soil [g/m2]
  real(kind=kind_phys), allocatable :: xlaixy    (:) ! leaf area index
  real(kind=kind_phys), allocatable :: xsaixy    (:) ! stem area index
  real(kind=kind_phys), allocatable :: taussxy   (:) ! snow age factor
  real(kind=kind_phys), allocatable :: smcwtdxy  (:) ! soil moisture content in the layer to the water table when deep
  real(kind=kind_phys), allocatable :: deeprechxy(:) ! recharge to the water table when deep
  real(kind=kind_phys), allocatable :: rechxy    (:) ! recharge to the water table (diagnostic) 
  real(kind=kind_phys), allocatable :: sncovr1   (:) ! snow cover over land (fractional)            im
  real(kind=kind_phys), allocatable :: qsurf     (:) ! specific humidity at sfc                     im
  real(kind=kind_phys), allocatable :: gflux     (:) ! soil heat flux (w/m**2)                      im
  real(kind=kind_phys), allocatable :: drain     (:) ! subsurface runoff (mm/s)                     im
  real(kind=kind_phys), allocatable :: evap      (:) ! evaperation from latent heat flux            im
  real(kind=kind_phys), allocatable :: hflx      (:) ! sensible heat flux                           im
  real(kind=kind_phys), allocatable :: ep        (:) ! potential evaporation                        im
  real(kind=kind_phys), allocatable :: runoff    (:) ! surface runoff (m/s)                         im
  real(kind=kind_phys), allocatable :: cmm       (:) !                                              im
  real(kind=kind_phys), allocatable :: chh       (:) !                                              im
  real(kind=kind_phys), allocatable :: evbs      (:) ! direct soil evaporation (m/s)                im
  real(kind=kind_phys), allocatable :: evcw      (:) ! canopy water evaporation (m/s)               im
  real(kind=kind_phys), allocatable :: sbsno     (:) ! sublimation/deposit from snopack (m/s)       im
  real(kind=kind_phys), allocatable :: snowc     (:) ! fractional snow cover                        im
  real(kind=kind_phys), allocatable :: stm       (:) ! total soil column moisture content (m)       im
  real(kind=kind_phys), allocatable :: snohf     (:) ! snow/freezing-rain latent heat flux (w/m**2) im
  real(kind=kind_phys), allocatable :: smcwlt2   (:) ! dry soil moisture threshold                  im
  real(kind=kind_phys), allocatable :: smcref2   (:) ! soil moisture threshold                      im
  real(kind=kind_phys), allocatable :: wet1      (:) ! normalized soil wetness                      im
  real(kind=kind_phys), allocatable :: t2mmp     (:) ! combined T2m from tiles                      im
  real(kind=kind_phys), allocatable :: q2mp      (:) ! combined q2m from tiles                      im

  real(kind=kind_phys), allocatable :: smc     (:,:) ! total soil moisture content (fractional)   im,km
  real(kind=kind_phys), allocatable :: stc     (:,:) ! soil temp (k)                              im,km
  real(kind=kind_phys), allocatable :: slc     (:,:) ! liquid soil moisture                       im,km
  real(kind=kind_phys), allocatable :: tsnoxy  (:,:) ! snow temperature [K]
  real(kind=kind_phys), allocatable :: zsnsoxy (:,:) ! snow layer depth [m]
  real(kind=kind_phys), allocatable :: snicexy (:,:) ! snow layer ice [mm]
  real(kind=kind_phys), allocatable :: snliqxy (:,:) ! snow layer liquid water [mm]
  real(kind=kind_phys), allocatable :: smoiseq (:,:) ! eq volumetric soil moisture [m3/m3]
 
end type noahmp_model_type

type, public :: noahmp_type

  type (noahmp_static_type) :: static
  type (noahmp_model_type)  :: model

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
    integer :: vector_length

    this%static%im = vector_length
    this%static%km = namelist%num_soil_levels

    allocate(this%model%u1        (vector_length))
    allocate(this%model%v1        (vector_length))
    allocate(this%model%soiltyp   (vector_length))
    allocate(this%model%vegtype   (vector_length))
    allocate(this%model%sigmaf    (vector_length))
    allocate(this%model%emiss     (vector_length))
    allocate(this%model%albdvis   (vector_length))
    allocate(this%model%albdnir   (vector_length))
    allocate(this%model%albivis   (vector_length))
    allocate(this%model%albinir   (vector_length))
    allocate(this%model%snet      (vector_length))
    allocate(this%model%tg3       (vector_length))
    allocate(this%model%cm        (vector_length))
    allocate(this%model%ch        (vector_length))
    allocate(this%model%prsl1     (vector_length))
    allocate(this%model%prslki    (vector_length))
    allocate(this%model%zf        (vector_length))
    allocate(this%model%dry       (vector_length))
    allocate(this%model%slopetyp  (vector_length))
    allocate(this%model%shdmin    (vector_length))
    allocate(this%model%shdmax    (vector_length))
    allocate(this%model%snoalb    (vector_length))
    allocate(this%model%sfalb     (vector_length))
    allocate(this%model%flag_iter (vector_length))
    allocate(this%model%flag_guess(vector_length))
    allocate(this%model%xlatin    (vector_length))
    allocate(this%model%xcoszin   (vector_length))
    allocate(this%model%rainn_mp  (vector_length))
    allocate(this%model%rainc_mp  (vector_length))
    allocate(this%model%snow_mp   (vector_length))
    allocate(this%model%graupel_mp(vector_length))
    allocate(this%model%ice_mp    (vector_length))
    allocate(this%model%weasd     (vector_length))
    allocate(this%model%snwdph    (vector_length))
    allocate(this%model%tskin     (vector_length))
    allocate(this%model%srflag    (vector_length))
    allocate(this%model%canopy    (vector_length))
    allocate(this%model%trans     (vector_length))
    allocate(this%model%tsurf     (vector_length))
    allocate(this%model%zorl      (vector_length))
    allocate(this%model%snowxy    (vector_length))
    allocate(this%model%tvxy      (vector_length))
    allocate(this%model%tgxy      (vector_length))
    allocate(this%model%canicexy  (vector_length))
    allocate(this%model%canliqxy  (vector_length))
    allocate(this%model%eahxy     (vector_length))
    allocate(this%model%tahxy     (vector_length))
    allocate(this%model%cmxy      (vector_length))
    allocate(this%model%chxy      (vector_length))
    allocate(this%model%fwetxy    (vector_length))
    allocate(this%model%sneqvoxy  (vector_length))
    allocate(this%model%alboldxy  (vector_length))
    allocate(this%model%qsnowxy   (vector_length))
    allocate(this%model%wslakexy  (vector_length))
    allocate(this%model%zwtxy     (vector_length))
    allocate(this%model%waxy      (vector_length))
    allocate(this%model%wtxy      (vector_length))
    allocate(this%model%lfmassxy  (vector_length))
    allocate(this%model%rtmassxy  (vector_length))
    allocate(this%model%stmassxy  (vector_length))
    allocate(this%model%woodxy    (vector_length))
    allocate(this%model%stblcpxy  (vector_length))
    allocate(this%model%fastcpxy  (vector_length))
    allocate(this%model%xlaixy    (vector_length))
    allocate(this%model%xsaixy    (vector_length))
    allocate(this%model%taussxy   (vector_length))
    allocate(this%model%smcwtdxy  (vector_length))
    allocate(this%model%deeprechxy(vector_length))
    allocate(this%model%rechxy    (vector_length))
    allocate(this%model%sncovr1   (vector_length))
    allocate(this%model%qsurf     (vector_length))
    allocate(this%model%gflux     (vector_length))
    allocate(this%model%drain     (vector_length))
    allocate(this%model%evap      (vector_length))
    allocate(this%model%hflx      (vector_length))
    allocate(this%model%ep        (vector_length))
    allocate(this%model%runoff    (vector_length))
    allocate(this%model%cmm       (vector_length))
    allocate(this%model%chh       (vector_length))
    allocate(this%model%evbs      (vector_length))
    allocate(this%model%evcw      (vector_length))
    allocate(this%model%sbsno     (vector_length))
    allocate(this%model%snowc     (vector_length))
    allocate(this%model%stm       (vector_length))
    allocate(this%model%snohf     (vector_length))
    allocate(this%model%smcwlt2   (vector_length))
    allocate(this%model%smcref2   (vector_length))
    allocate(this%model%wet1      (vector_length))
    allocate(this%model%t2mmp     (vector_length))
    allocate(this%model%q2mp      (vector_length))

    allocate(this%model%smc       (vector_length,namelist%num_soil_levels))
    allocate(this%model%stc       (vector_length,namelist%num_soil_levels))
    allocate(this%model%slc       (vector_length,namelist%num_soil_levels))
    allocate(this%model%smoiseq   (vector_length,namelist%num_soil_levels))
    allocate(this%model%tsnoxy    (vector_length,-2:0))
    allocate(this%model%zsnsoxy   (vector_length,-2:namelist%num_soil_levels))
    allocate(this%model%snicexy   (vector_length,-2:0))
    allocate(this%model%snliqxy   (vector_length,-2:0))

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(noahmp_type) :: this

    this%static%itime      = huge(1)
    this%static%con_hvap   = huge(1.0)
    this%static%con_cp     = huge(1.0)
    this%static%con_jcal   = huge(1.0)
    this%static%rhoh2o     = huge(1.0)
    this%static%con_eps    = huge(1.0)
    this%static%con_epsm1  = huge(1.0)
    this%static%con_fvirt  = huge(1.0)
    this%static%con_rd     = huge(1.0)
    this%static%con_hfus   = huge(1.0)
    this%static%delt       = huge(1.0)
    this%static%idveg      = huge(1)
    this%static%iopt_crs   = huge(1)
    this%static%iopt_btr   = huge(1)
    this%static%iopt_run   = huge(1)
    this%static%iopt_sfc   = huge(1)
    this%static%iopt_frz   = huge(1)
    this%static%iopt_inf   = huge(1)
    this%static%iopt_rad   = huge(1)
    this%static%iopt_alb   = huge(1)
    this%static%iopt_snf   = huge(1)
    this%static%iopt_tbot  = huge(1)
    this%static%iopt_stc   = huge(1)
    this%static%iopt_rsf   = huge(1)
    this%static%iopt_gla   = huge(1)
    this%static%errmsg     = ""
    this%static%errflg     = huge(1)

    this%model%iyrlen     = huge(1)
    this%model%julian     = huge(1.0)
    this%model%u1         = huge(1.0)
    this%model%v1         = huge(1.0)
    this%model%soiltyp    = huge(1)
    this%model%vegtype    = huge(1)
    this%model%sigmaf     = huge(1.0)
    this%model%emiss      = huge(1.0)
    this%model%albdvis    = huge(1.0)
    this%model%albdnir    = huge(1.0)
    this%model%albivis    = huge(1.0)
    this%model%albinir    = huge(1.0)
    this%model%snet       = huge(1.0)
    this%model%tg3        = huge(1.0)
    this%model%cm         = huge(1.0)
    this%model%ch         = huge(1.0)
    this%model%prsl1      = huge(1.0)
    this%model%prslki     = huge(1.0)
    this%model%zf         = huge(1.0)
    this%model%dry        = .false.
    this%model%slopetyp   = huge(1)
    this%model%shdmin     = huge(1.0)
    this%model%shdmax     = huge(1.0)
    this%model%snoalb     = huge(1.0)
    this%model%sfalb      = huge(1.0)
    this%model%flag_iter  = .false.
    this%model%flag_guess = .false.
    this%model%xlatin     = huge(1.0)
    this%model%xcoszin    = huge(1.0)
    this%model%rainn_mp   = huge(1.0)
    this%model%rainc_mp   = huge(1.0)
    this%model%snow_mp    = huge(1.0)
    this%model%graupel_mp = huge(1.0)
    this%model%ice_mp     = huge(1.0)
    this%model%weasd      = huge(1.0)
    this%model%snwdph     = huge(1.0)
    this%model%tskin      = huge(1.0)
    this%model%srflag     = huge(1.0)
    this%model%canopy     = huge(1.0)
    this%model%trans      = huge(1.0)
    this%model%tsurf      = huge(1.0)
    this%model%zorl       = huge(1.0)
    this%model%snowxy     = huge(1.0)
    this%model%tvxy       = huge(1.0)
    this%model%tgxy       = huge(1.0)
    this%model%canicexy   = huge(1.0)
    this%model%canliqxy   = huge(1.0)
    this%model%eahxy      = huge(1.0)
    this%model%tahxy      = huge(1.0)
    this%model%cmxy       = huge(1.0)
    this%model%chxy       = huge(1.0)
    this%model%fwetxy     = huge(1.0)
    this%model%sneqvoxy   = huge(1.0)
    this%model%alboldxy   = huge(1.0)
    this%model%qsnowxy    = huge(1.0)
    this%model%wslakexy   = huge(1.0)
    this%model%zwtxy      = huge(1.0)
    this%model%waxy       = huge(1.0)
    this%model%wtxy       = huge(1.0)
    this%model%lfmassxy   = huge(1.0)
    this%model%rtmassxy   = huge(1.0)
    this%model%stmassxy   = huge(1.0)
    this%model%woodxy     = huge(1.0)
    this%model%stblcpxy   = huge(1.0)
    this%model%fastcpxy   = huge(1.0)
    this%model%xlaixy     = huge(1.0)
    this%model%xsaixy     = huge(1.0)
    this%model%taussxy    = huge(1.0)
    this%model%smoiseq    = huge(1.0)
    this%model%smcwtdxy   = huge(1.0)
    this%model%deeprechxy = huge(1.0)
    this%model%rechxy     = huge(1.0)
    this%model%sncovr1    = huge(1.0)
    this%model%qsurf      = huge(1.0)
    this%model%gflux      = huge(1.0)
    this%model%drain      = huge(1.0)
    this%model%evap       = huge(1.0)
    this%model%hflx       = huge(1.0)
    this%model%ep         = huge(1.0)
    this%model%runoff     = huge(1.0)
    this%model%cmm        = huge(1.0)
    this%model%chh        = huge(1.0)
    this%model%evbs       = huge(1.0)
    this%model%evcw       = huge(1.0)
    this%model%sbsno      = huge(1.0)
    this%model%snowc      = huge(1.0)
    this%model%stm        = huge(1.0)
    this%model%snohf      = huge(1.0)
    this%model%smcwlt2    = huge(1.0)
    this%model%smcref2    = huge(1.0)
    this%model%wet1       = huge(1.0)
    this%model%t2mmp      = huge(1.0)
    this%model%q2mp       = huge(1.0)
    this%model%smc        = huge(1.0)
    this%model%stc        = huge(1.0)
    this%model%slc        = huge(1.0)
    this%model%smoiseq    = huge(1.0)
    this%model%tsnoxy     = huge(1.0)
    this%model%zsnsoxy    = huge(1.0)
    this%model%snicexy    = huge(1.0)
    this%model%snliqxy    = huge(1.0)

  end subroutine InitDefault

  subroutine TransferNamelist(this, namelist)
    
  class(noahmp_type)     :: this
  type(namelist_type)  :: namelist
  
  this%static%delt           = namelist%timestep_seconds
  this%model%zf              = namelist%forcing_height
  this%static%idveg          = namelist%dynamic_vegetation_option
  this%static%iopt_crs       = namelist%canopy_stomatal_resistance_option
  this%static%iopt_btr       = namelist%soil_wetness_option
  this%static%iopt_run       = namelist%runoff_option
  this%static%iopt_sfc       = namelist%surface_exchange_option
  this%static%iopt_frz       = namelist%supercooled_soilwater_option
  this%static%iopt_inf       = namelist%frozen_soil_adjust_option
  this%static%iopt_rad       = namelist%radiative_transfer_option
  this%static%iopt_alb       = namelist%snow_albedo_option
  this%static%iopt_snf       = namelist%precip_partition_option
  this%static%iopt_tbot      = namelist%soil_temp_lower_bdy_option
  this%static%iopt_stc       = namelist%soil_temp_time_scheme_option
  this%static%iopt_rsf       = namelist%surface_evap_resistance_option
  this%static%iopt_gla       = namelist%glacier_option
  
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
  real                 :: masslai, masssai, depthm, dzsno(-2:0)
  integer              :: ilevel, iloc, isnow

  call date_from_since(namelist%reference_date, now_time, current_date)
  read(current_date( 6: 7),  '(i2)') current_mm
  
  do iloc = 1, this%static%im
  
    this%model%tvxy (iloc) = this%model%tskin(iloc)
    this%model%tgxy (iloc) = this%model%tskin(iloc)
    this%model%tahxy(iloc) = this%model%tskin(iloc)

    if (this%model%snwdph(iloc) > 0.01 .and. this%model%tskin(iloc) > 273.15 ) then
      this%model%tvxy (iloc) = 273.15
      this%model%tgxy (iloc) = 273.15
      this%model%tahxy(iloc) = 273.15
    end if
  
    this%model%canicexy(iloc) = 0.0
    this%model%canliqxy(iloc) = this%model%canopy(iloc)

    this%model%eahxy(iloc)    = 2000.0

    this%model%cmxy    (iloc) = 0.0
    this%model%chxy    (iloc) = 0.0
    this%model%fwetxy  (iloc) = 0.0
    this%model%sneqvoxy(iloc) = this%model%weasd(iloc)
    this%model%alboldxy(iloc) = 0.65
    this%model%qsnowxy (iloc) = 0.0

    this%model%wslakexy(iloc) = 0.0
    this%model%taussxy (iloc) = 0.0

    this%model%waxy    (iloc) = 4900.0                ! this assumes water table is at 2.5m
    this%model%wtxy    (iloc) = this%model%waxy(iloc)
    this%model%zwtxy   (iloc) = (25.0 + 2.0) - this%model%waxy(iloc) / 1000.0 /0.2

    if ((this%model%vegtype(iloc) == isbarren_table) .or. (this%model%vegtype(iloc) == isice_table)   .or. &
        (this%model%vegtype(iloc) == isurban_table)  .or. (this%model%vegtype(iloc) == iswater_table)) then

      this%model%xlaixy  (iloc) = 0.0
      this%model%xsaixy  (iloc) = 0.0

      this%model%lfmassxy(iloc) = 0.0
      this%model%stmassxy(iloc) = 0.0
      this%model%rtmassxy(iloc) = 0.0

      this%model%woodxy  (iloc) = 0.0       
      this%model%stblcpxy(iloc) = 0.0      
      this%model%fastcpxy(iloc) = 0.0     

    else

      this%model%xlaixy  (iloc) = max(laim_table(this%model%vegtype(iloc), current_mm),0.05)
      this%model%xsaixy  (iloc) = max(this%model%xlaixy(iloc)*0.1,0.05)

      masslai                   = 1000.0 / max(sla_table(this%model%vegtype(iloc)),1.0)
      this%model%lfmassxy(iloc) = this%model%xlaixy(iloc) * masslai
      masssai                   = 1000.0 / 3.0
      this%model%stmassxy(iloc) = this%model%xsaixy(iloc) * masssai

      this%model%rtmassxy(iloc) = 500.0      
      this%model%woodxy  (iloc) = 500.0       
      this%model%stblcpxy(iloc) = 1000.0      
      this%model%fastcpxy(iloc) = 1000.0     

    end if

    if (this%model%vegtype(iloc)  == isice_table )  then
      do ilevel = 1, namelist%num_soil_levels
        this%model%stc(iloc,ilevel) = min(this%model%stc(iloc,ilevel),min(this%model%tg3(iloc),263.15))
      end do
      this%model%smc(iloc,:) = 1.0
      this%model%slc(iloc,:) = 0.0
    end if

    depthm = this%model%snwdph(iloc)/1000.0  ! snow depth in meters

    if (this%model%weasd(iloc) /= 0.0 .and. depthm == 0.0 ) then
      depthm = this%model%weasd(iloc) / 1000.0 * 10.0   ! assume 10/1 ratio
    endif

    if (this%model%vegtype(iloc) == 15) then  ! land ice in MODIS/IGBP
      if (this%model%weasd(iloc) < 0.1) then
        this%model%weasd(iloc) = 0.1
        depthm                 = this%model%weasd(iloc) / 1000.0 * 10.0 
      end if
    end if

    dzsno = 0.0
    if (depthm < 0.025 ) then
      this%model%snowxy(iloc)  = 0.0
      dzsno(-2:0)              = 0.0
    elseif (depthm >= 0.025 .and. depthm <= 0.05 ) then
      this%model%snowxy(iloc)  = -1.0
      dzsno(0)                 = depthm
    elseif (depthm > 0.05 .and. depthm <= 0.10 ) then
      this%model%snowxy(iloc)  = -2.0
      dzsno(-1)                = 0.5*depthm
      dzsno(0)                 = 0.5*depthm
    elseif (depthm > 0.10 .and. depthm <= 0.25 ) then
      this%model%snowxy(iloc)  = -2.0
      dzsno(-1)                = 0.05
      dzsno(0)                 = depthm - 0.05
    elseif (depthm > 0.25 .and. depthm <= 0.45 ) then
      this%model%snowxy(iloc)  = -3.0
      dzsno(-2)                = 0.05
      dzsno(-1)                = 0.5*(depthm-0.05)
      dzsno(0)                 = 0.5*(depthm-0.05)
    elseif (depthm > 0.45) then 
      this%model%snowxy(iloc)  = -3.0
      dzsno(-2)                = 0.05
      dzsno(-1)                = 0.20
      dzsno(0)                 = depthm - 0.05 - 0.20
    else
      ! call mpp_error(FATAL, 'problem with the logic assigning snow layers.') 
    endif

! Now we have the snowxy field
! snice + snliq + tsno allocation and compute them from what we have
             
    this%model%tsnoxy (iloc,-2:0) = 0.0
    this%model%snicexy(iloc,-2:0) = 0.0
    this%model%snliqxy(iloc,-2:0) = 0.0
    this%model%zsnsoxy(iloc,-2:namelist%num_soil_levels) = 0.0

    isnow = nint(this%model%snowxy(iloc))+1    ! snowxy <=0.0, dzsno >= 0.0

    do ilevel = isnow , 0
      this%model%tsnoxy (iloc,ilevel) = this%model%tgxy(iloc)
      this%model%snliqxy(iloc,ilevel) = 0.0
      this%model%snicexy(iloc,ilevel) = dzsno(ilevel) / depthm * this%model%weasd(iloc)
    end do

    this%model%zsnsoxy(iloc,isnow) = -1.0*dzsno(isnow)
    do ilevel = isnow+1, 0
      this%model%zsnsoxy(iloc,ilevel) = this%model%zsnsoxy(iloc,ilevel-1) - dzsno(ilevel)
    end do
    do ilevel = 1, namelist%num_soil_levels
      this%model%zsnsoxy(iloc,ilevel) = this%model%zsnsoxy(iloc,ilevel-1) - namelist%soil_level_thickness(ilevel)
    end do

! Should not need to initialize these
    
    this%model%smoiseq(iloc,:)  = 0.0
    this%model%smcwtdxy(iloc)   = 0.0
    this%model%deeprechxy(iloc) = 0.0
    this%model%rechxy(iloc)     = 0.0

  end do ! iloc
  
  end subroutine InitStates

end module ufsLandNoahMPType
