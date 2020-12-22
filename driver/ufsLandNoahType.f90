module ufsLandNoahType

use NamelistRead
use machine , only : kind_phys

implicit none
save
private

type :: noah_static_type

  integer                 ::   im         ! horiz dimension and num of used pts         1
  integer                 ::   km         ! vertical soil layer dimension               1
  real(kind=kind_phys)    ::   grav       ! constant added to call in ccpp
  real(kind=kind_phys)    ::   cp         ! constant added to call in ccpp
  real(kind=kind_phys)    ::   hvap       ! constant added to call in ccpp
  real(kind=kind_phys)    ::   rd         ! constant added to call in ccpp
  real(kind=kind_phys)    ::   eps        ! constant added to call in ccpp
  real(kind=kind_phys)    ::   epsm1      ! constant added to call in ccpp
  real(kind=kind_phys)    ::   rvrdm1     ! constant added to call in ccpp
  real(kind=kind_phys)    ::   delt       ! time interval (second)                      1
  integer                 ::   isot       ! sfc soil type data source zobler or statsgo
  integer                 ::   ivegsrc    ! sfc veg type data source umd or igbp
  logical                 ::   lheatstrg  ! flag for canopy heat storage parameterization  1
  character(len=128)      ::   errmsg     ! error messaging added to ccpp
  integer                 ::   errflg     ! error messaging added to ccpp
  real(kind=kind_phys)    ::   pertvegf

end type noah_static_type

type :: noah_model_type

  real(kind=kind_phys), allocatable :: ps        (:) ! surface pressure (pa)                       im 
  real(kind=kind_phys), allocatable :: t1        (:) ! surface layer mean temperature (k)          im
  real(kind=kind_phys), allocatable :: q1        (:) ! surface layer mean specific humidity        im
  integer             , allocatable :: soiltyp   (:) ! soil type (integer index)                   im
  integer             , allocatable :: vegtype   (:) ! vegetation type (integer index)             im
  real(kind=kind_phys), allocatable :: sigmaf    (:) ! areal fractional cover of green vegetation  im
  real(kind=kind_phys), allocatable :: sfcemis   (:) ! sfc lw emissivity ( fraction )              im
  real(kind=kind_phys), allocatable :: dlwflx    (:) ! total sky sfc downward lw flux ( w/m**2 )   im
  real(kind=kind_phys), allocatable :: dswsfc    (:) ! total sky sfc downward sw flux ( w/m**2 )   im
  real(kind=kind_phys), allocatable :: snet      (:) ! total sky sfc netsw flx into ground(w/m**2) im
  real(kind=kind_phys), allocatable :: tg3       (:) ! deep soil temperature (k)                   im
  real(kind=kind_phys), allocatable :: cm        (:) ! surface exchange coeff for momentum (m/s)   im
  real(kind=kind_phys), allocatable :: ch        (:) ! surface exchange coeff heat & moisture(m/s) im
  real(kind=kind_phys), allocatable :: prsl1     (:) ! sfc layer 1 mean pressure (pa)              im
  real(kind=kind_phys), allocatable :: prslki    (:) !                                             im
  real(kind=kind_phys), allocatable :: zf        (:) ! height of bottom layer (m)                  im
  logical             , allocatable :: land      (:) ! = T if a point with any land                im
  real(kind=kind_phys), allocatable :: wind      (:) ! wind speed (m/s)                            im
  integer             , allocatable :: slopetyp  (:) ! class of sfc slope (integer index)          im
  real(kind=kind_phys), allocatable :: shdmin    (:) ! min fractional coverage of green veg        im
  real(kind=kind_phys), allocatable :: shdmax    (:) ! max fractnl cover of green veg (not used)   im
  real(kind=kind_phys), allocatable :: snoalb    (:) ! upper bound on max albedo over deep snow    im
  real(kind=kind_phys), allocatable :: sfalb     (:) ! mean sfc diffused sw albedo (fractional)    im
  logical             , allocatable :: flag_iter (:) !                                             im
  logical             , allocatable :: flag_guess(:) !                                             im
  real(kind=kind_phys), allocatable :: bexppert  (:)
  real(kind=kind_phys), allocatable :: xlaipert  (:)
  real(kind=kind_phys), allocatable :: vegfpert  (:)
  real(kind=kind_phys), allocatable :: weasd     (:) ! water equivalent accumulated snow depth(mm) im
  real(kind=kind_phys), allocatable :: snwdph    (:) ! snow depth (water equiv) over land          im
  real(kind=kind_phys), allocatable :: tskin     (:) ! ground surface skin temperature ( k )       im
  real(kind=kind_phys), allocatable :: tprcp     (:) ! total precipitation                         im
  real(kind=kind_phys), allocatable :: srflag    (:) ! snow/rain flag for precipitation            im
  real(kind=kind_phys), allocatable :: canopy    (:) ! canopy moisture content (m)                 im
  real(kind=kind_phys), allocatable :: trans     (:) ! total plant transpiration (m/s)             im
  real(kind=kind_phys), allocatable :: tsurf     (:) ! surface skin temperature (after iteration)  im
  real(kind=kind_phys), allocatable :: zorl      (:) ! surface roughness                           im
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

  real(kind=kind_phys), allocatable :: smc(:,:) ! total soil moisture content (fractional)   im,km
  real(kind=kind_phys), allocatable :: stc(:,:) ! soil temp (k)                              im,km
  real(kind=kind_phys), allocatable :: slc(:,:) ! liquid soil moisture                       im,km
 
end type noah_model_type

type, public :: noah_type

  type (noah_static_type) :: static
  type (noah_model_type)  :: model

  contains

    procedure, public  :: Init         
    procedure, private :: InitAllocate 
    procedure, private :: InitDefault     
    procedure, public  :: TransferNamelist         

end type noah_type

contains   

  subroutine Init(this, namelist, vector_length)

    class(noah_type) :: this
    type(namelist_type) :: namelist
    integer :: vector_length

    call this%InitAllocate(namelist,vector_length)
    call this%InitDefault()

  end subroutine Init

  subroutine InitAllocate(this, namelist, vector_length)

    class(noah_type) :: this
    type(namelist_type) :: namelist
    integer :: vector_length

    this%static%im = vector_length
    this%static%km = namelist%num_soil_levels

    allocate(this%model%ps        (vector_length))
    allocate(this%model%t1        (vector_length))
    allocate(this%model%q1        (vector_length))
    allocate(this%model%soiltyp   (vector_length))
    allocate(this%model%vegtype   (vector_length))
    allocate(this%model%sigmaf    (vector_length))
    allocate(this%model%sfcemis   (vector_length))
    allocate(this%model%dlwflx    (vector_length))
    allocate(this%model%dswsfc    (vector_length))
    allocate(this%model%snet      (vector_length))
    allocate(this%model%tg3       (vector_length))
    allocate(this%model%cm        (vector_length))
    allocate(this%model%ch        (vector_length))
    allocate(this%model%prsl1     (vector_length))
    allocate(this%model%prslki    (vector_length))
    allocate(this%model%zf        (vector_length))
    allocate(this%model%land      (vector_length))
    allocate(this%model%wind      (vector_length))
    allocate(this%model%slopetyp  (vector_length))
    allocate(this%model%shdmin    (vector_length))
    allocate(this%model%shdmax    (vector_length))
    allocate(this%model%snoalb    (vector_length))
    allocate(this%model%sfalb     (vector_length))
    allocate(this%model%flag_iter (vector_length))
    allocate(this%model%flag_guess(vector_length))
    allocate(this%model%bexppert  (vector_length))
    allocate(this%model%xlaipert  (vector_length))
    allocate(this%model%vegfpert  (vector_length))
    allocate(this%model%weasd     (vector_length))
    allocate(this%model%snwdph    (vector_length))
    allocate(this%model%tskin     (vector_length))
    allocate(this%model%tprcp     (vector_length))
    allocate(this%model%srflag    (vector_length))
    allocate(this%model%canopy    (vector_length))
    allocate(this%model%trans     (vector_length))
    allocate(this%model%tsurf     (vector_length))
    allocate(this%model%zorl      (vector_length))
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

    allocate(this%model%smc  (vector_length,namelist%num_soil_levels))
    allocate(this%model%stc  (vector_length,namelist%num_soil_levels))
    allocate(this%model%slc  (vector_length,namelist%num_soil_levels))

  end subroutine InitAllocate

  subroutine InitDefault(this)

    class(noah_type) :: this

    this%static%grav       = huge(1.0)
    this%static%cp         = huge(1.0)
    this%static%hvap       = huge(1.0)
    this%static%rd         = huge(1.0)
    this%static%eps        = huge(1.0)
    this%static%epsm1      = huge(1.0)
    this%static%rvrdm1     = huge(1.0)
    this%static%delt       = huge(1.0)
    this%static%isot       = huge(1)
    this%static%ivegsrc    = huge(1)
    this%static%lheatstrg  = .false.
    this%static%errmsg     = ""
    this%static%errflg     = huge(1)
    this%static%pertvegf   = huge(1.0)

    this%model%ps         = huge(1.0)
    this%model%t1         = huge(1.0)
    this%model%q1         = huge(1.0)
    this%model%soiltyp    = huge(1)
    this%model%vegtype    = huge(1)
    this%model%sigmaf     = huge(1.0)
    this%model%sfcemis    = huge(1.0)
    this%model%dlwflx     = huge(1.0)
    this%model%dswsfc     = huge(1.0)
    this%model%snet       = huge(1.0)
    this%model%tg3        = huge(1.0)
    this%model%cm         = huge(1.0)
    this%model%ch         = huge(1.0)
    this%model%prsl1      = huge(1.0)
    this%model%prslki     = huge(1.0)
    this%model%zf         = huge(1.0)
    this%model%land       = .false.
    this%model%wind       = huge(1.0)
    this%model%slopetyp   = huge(1)
    this%model%shdmin     = huge(1.0)
    this%model%shdmax     = huge(1.0)
    this%model%snoalb     = huge(1.0)
    this%model%sfalb      = huge(1.0)
    this%model%flag_iter  = .false.
    this%model%flag_guess = .false.
    this%model%bexppert   = huge(1.0)
    this%model%xlaipert   = huge(1.0)
    this%model%vegfpert   = huge(1.0)
    this%model%weasd      = huge(1.0)
    this%model%snwdph     = huge(1.0)
    this%model%tskin      = huge(1.0)
    this%model%tprcp      = huge(1.0)
    this%model%srflag     = huge(1.0)
    this%model%canopy     = huge(1.0)
    this%model%trans      = huge(1.0)
    this%model%tsurf      = huge(1.0)
    this%model%zorl       = huge(1.0)
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
    this%model%smc        = huge(1.0)
    this%model%stc        = huge(1.0)
    this%model%slc        = huge(1.0)

  end subroutine InitDefault

  subroutine TransferNamelist(this, namelist)
    
  class(noah_type)     :: this
  type(namelist_type)  :: namelist
  
  this%static%delt    = namelist%timestep_seconds
  this%model%zf       = namelist%forcing_height
  
  end subroutine TransferNamelist

end module ufsLandNoahType
