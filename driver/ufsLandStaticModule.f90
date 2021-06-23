module ufsLandStaticModule

use NamelistRead

implicit none
save
private

type, public :: static_type

  integer                               :: nlocations
  integer                               :: nlevels
  real   , allocatable, dimension(:)    :: latitude
  real   , allocatable, dimension(:)    :: longitude
  integer, allocatable, dimension(:)    :: vegetation_category
  integer, allocatable, dimension(:)    :: soil_category
  integer, allocatable, dimension(:)    :: slope_category
  real   , allocatable, dimension(:)    :: deep_soil_temperature
  real   , allocatable, dimension(:)    :: elevation
  integer, allocatable, dimension(:)    :: land_mask
  real   , allocatable, dimension(:)    :: soil_level_thickness
  real   , allocatable, dimension(:)    :: soil_level_nodes
  real   , allocatable, dimension(:)    :: max_snow_albedo
  real   , allocatable, dimension(:)    :: emissivity
  real   , allocatable, dimension(:,:)  :: gvf_monthly
  real   , allocatable, dimension(:,:)  :: albedo_monthly
  real   , allocatable, dimension(:,:)  :: lai_monthly
  real   , allocatable, dimension(:,:)  :: z0_monthly
  integer                               :: iswater
  integer                               :: isurban
  integer                               :: isice
  character*100                         :: land_cover_source
  character*100                         :: soil_class_source

  contains

    procedure, public  :: ReadStatic
    procedure, public  :: TransferStaticNoah
    procedure, public  :: TransferStaticNoahMP

end type static_type
     
contains   

  subroutine ReadStatic(this, namelist)
  
  use netcdf
  use error_handling, only : handle_err
  
  class(static_type)  :: this
  type(namelist_type) :: namelist
  
  integer :: ncid, dimid, varid, status
  
  status = nf90_open(namelist%static_file, NF90_NOWRITE, ncid)
   if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_dimid(ncid, "location", dimid)
   if (status /= nf90_noerr) call handle_err(status)

  status = nf90_inquire_dimension(ncid, dimid, len = this%nlocations)
   if (status /= nf90_noerr) call handle_err(status)
  
  if(namelist%begloc > this%nlocations .or. namelist%endloc > this%nlocations) &
    stop "begloc or endloc not consistent with nlocations in static read"
   
  status = nf90_inq_dimid(ncid, "soil_levels", dimid)
   if (status /= nf90_noerr) call handle_err(status)

  status = nf90_inquire_dimension(ncid, dimid, len = this%nlevels)
   if (status /= nf90_noerr) call handle_err(status)
  
  allocate(this%latitude              (namelist%lensub))
  allocate(this%longitude             (namelist%lensub))
  allocate(this%vegetation_category   (namelist%lensub))
  allocate(this%soil_category         (namelist%lensub))
  allocate(this%slope_category        (namelist%lensub))
  allocate(this%deep_soil_temperature (namelist%lensub))
  allocate(this%elevation             (namelist%lensub))
  allocate(this%land_mask             (namelist%lensub))
  allocate(this%soil_level_thickness  (this%nlevels))
  allocate(this%soil_level_nodes      (this%nlevels))
  allocate(this%max_snow_albedo       (namelist%lensub))
  allocate(this%emissivity            (namelist%lensub))
  allocate(this%gvf_monthly           (namelist%lensub,12))
  allocate(this%albedo_monthly        (namelist%lensub,12))
  allocate(this%lai_monthly           (namelist%lensub,12))
  allocate(this%z0_monthly            (namelist%lensub,12))
  
  status = nf90_inq_varid(ncid, "latitude", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%latitude, &
       start = (/namelist%begsub/), count = (/namelist%lensub/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "longitude", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%longitude, &
       start = (/namelist%begsub/), count = (/namelist%lensub/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "vegetation_category", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%vegetation_category, &
       start = (/namelist%begsub/), count = (/namelist%lensub/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "soil_category", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%soil_category, &
       start = (/namelist%begsub/), count = (/namelist%lensub/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "slope_category", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%slope_category, &
       start = (/namelist%begsub/), count = (/namelist%lensub/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_inq_varid(ncid, "deep_soil_temperature", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%deep_soil_temperature, &
       start = (/namelist%begsub/), count = (/namelist%lensub/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "elevation", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%elevation, &
       start = (/namelist%begsub/), count = (/namelist%lensub/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "land_mask", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%land_mask, &
       start = (/namelist%begsub/), count = (/namelist%lensub/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "soil_level_thickness", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%soil_level_thickness)
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "soil_level_nodes", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%soil_level_nodes)
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "max_snow_albedo", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%max_snow_albedo, &
       start = (/namelist%begsub/), count = (/namelist%lensub/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "emissivity", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%emissivity, &
       start = (/namelist%begsub/), count = (/namelist%lensub/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "gvf_monthly", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%gvf_monthly, &
       start = (/namelist%begsub,1/), count = (/namelist%lensub,12/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_inq_varid(ncid, "albedo_monthly", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%albedo_monthly, &
       start = (/namelist%begsub,1/), count = (/namelist%lensub,12/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_inq_varid(ncid, "lai_monthly", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%lai_monthly, &
       start = (/namelist%begsub,1/), count = (/namelist%lensub,12/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_inq_varid(ncid, "z0_monthly", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%z0_monthly, &
       start = (/namelist%begsub,1/), count = (/namelist%lensub,12/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_get_att(ncid, NF90_GLOBAL, "iswater", this%iswater)
  status = nf90_get_att(ncid, NF90_GLOBAL, "isice"  , this%isice)
  status = nf90_get_att(ncid, NF90_GLOBAL, "isurban", this%isurban)
  status = nf90_get_att(ncid, NF90_GLOBAL, "land_cover_source", this%land_cover_source)
  status = nf90_get_att(ncid, NF90_GLOBAL, "soil_class_source", this%soil_class_source)
  
  end subroutine ReadStatic

  subroutine TransferStaticNoah(this, noah)
  
  use ufsLandNoahType
  
  class(static_type)   :: this
  type(noah_type)      :: noah
  
  noah%static%isot    = 1
  noah%static%ivegsrc = 1
  noah%model%soiltyp  = this%soil_category
  noah%model%vegtype  = this%vegetation_category
  noah%model%slopetyp = this%slope_category
  noah%model%tg3      = this%deep_soil_temperature
  noah%model%sfcemis  = this%emissivity
  noah%model%snoalb   = this%max_snow_albedo
  noah%model%land     = .true.
  noah%model%shdmin   = minval(this%gvf_monthly,dim=2)
  noah%model%shdmax   = maxval(this%gvf_monthly,dim=2)

  noah%model%land     = .false.
  where(this%vegetation_category /= this%iswater) noah%model%land = .true.
  
  end subroutine TransferStaticNoah

  subroutine TransferStaticNoahMP(this, noahmp)
  
  use ufsLandNoahMPType
  
  class(static_type)  :: this
  type(noahmp_type)   :: noahmp
  
  noahmp%static%soil_source          = 1
  noahmp%static%veg_source           = 1
  noahmp%static%soil_category        = this%soil_category
  noahmp%static%vegetation_category  = this%vegetation_category
  noahmp%static%slope_category       = this%slope_category
  noahmp%static%temperature_soil_bot = this%deep_soil_temperature
  noahmp%model%max_vegetation_frac   = maxval(this%gvf_monthly,dim=2)
  noahmp%model%latitude              = this%latitude

  end subroutine TransferStaticNoahMP

end module ufsLandStaticModule
