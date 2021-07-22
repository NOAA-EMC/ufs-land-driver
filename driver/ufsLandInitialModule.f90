module ufsLandInitialModule

use NamelistRead

implicit none
save
private

type, public :: initial_type

  integer                            :: nlocations
  integer                            :: nlevels
  double precision                   :: time
  character*19                       :: date
  real, allocatable, dimension(:)    :: latitude
  real, allocatable, dimension(:)    :: longitude
  real, allocatable, dimension(:)    :: snow_water_equivalent
  real, allocatable, dimension(:)    :: snow_depth
  real, allocatable, dimension(:)    :: canopy_water
  real, allocatable, dimension(:)    :: skin_temperature
  real, allocatable, dimension(:)    :: soil_level_thickness
  real, allocatable, dimension(:)    :: soil_level_nodes
  real, allocatable, dimension(:,:)  :: soil_temperature
  real, allocatable, dimension(:,:)  :: soil_moisture
  real, allocatable, dimension(:,:)  :: soil_liquid
  integer                            :: iswater
  integer                            :: isurban
  integer                            :: isice
  character*100                      :: land_cover_source

  contains

    procedure, public  :: ReadInitial
    procedure, public  :: TransferInitialNoah
    procedure, public  :: TransferInitialNoahMP

end type initial_type
     
contains   

  subroutine ReadInitial(this, namelist)
  
  use netcdf
  use error_handling, only : handle_err
  
  class(initial_type)  :: this
  type(namelist_type)  :: namelist
  
  integer :: ncid, dimid, varid, status
  
  status = nf90_open(namelist%init_file, NF90_NOWRITE, ncid)
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
   
  allocate(this%latitude             (namelist%lensub))
  allocate(this%longitude            (namelist%lensub))
  allocate(this%snow_water_equivalent(namelist%lensub))
  allocate(this%snow_depth           (namelist%lensub))
  allocate(this%canopy_water         (namelist%lensub))
  allocate(this%skin_temperature     (namelist%lensub))
  allocate(this%soil_level_thickness (this%nlevels))
  allocate(this%soil_level_nodes     (this%nlevels))
  allocate(this%soil_temperature     (namelist%lensub,this%nlevels))
  allocate(this%soil_moisture        (namelist%lensub,this%nlevels))
  allocate(this%soil_liquid          (namelist%lensub,this%nlevels))
  
  status = nf90_inq_varid(ncid, "time", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%time)
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "date", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%date)
   if(status /= nf90_noerr) call handle_err(status)
  
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
  
  status = nf90_inq_varid(ncid, "snow_water_equivalent", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%snow_water_equivalent, &
       start = (/namelist%begsub/), count = (/namelist%lensub/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "snow_depth", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%snow_depth, &
       start = (/namelist%begsub/), count = (/namelist%lensub/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "canopy_water", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%canopy_water, &
       start = (/namelist%begsub/), count = (/namelist%lensub/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_inq_varid(ncid, "skin_temperature", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%skin_temperature, &
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
  
  status = nf90_inq_varid(ncid, "soil_temperature", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%soil_temperature, &
       start = (/namelist%begsub,1/), count = (/namelist%lensub,this%nlevels/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_inq_varid(ncid, "soil_moisture", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%soil_moisture, &
       start = (/namelist%begsub,1/), count = (/namelist%lensub,this%nlevels/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_inq_varid(ncid, "soil_liquid", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%soil_liquid, &
       start = (/namelist%begsub,1/), count = (/namelist%lensub,this%nlevels/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_get_att(ncid, NF90_GLOBAL, "iswater", this%iswater)
  status = nf90_get_att(ncid, NF90_GLOBAL, "isice"  , this%isice)
  status = nf90_get_att(ncid, NF90_GLOBAL, "isurban", this%isurban)
  status = nf90_get_att(ncid, NF90_GLOBAL, "land_cover_source", this%land_cover_source)
  
  namelist%initial_time = this%time
  
  end subroutine ReadInitial

  subroutine TransferInitialNoah(this, namelist, noah)
  
  use ufsLandNoahType
  
  class(initial_type)  :: this
  type(namelist_type)  :: namelist
  type(noah_type)      :: noah
  
  if(namelist%lensub /= noah%static%im) stop "vector length mismatch in ufsLandInitial_TransferInitial"
  if(this%nlevels    /= noah%static%km) stop "  soil levels mismatch in ufsLandInitial_TransferInitial"
  
  noah%model%weasd  = this%snow_water_equivalent
  noah%model%snwdph = this%snow_depth * 1000.0  ! driver wants mm
  noah%model%canopy = this%canopy_water
  noah%model%tskin  = this%skin_temperature
  noah%model%stc    = this%soil_temperature
  noah%model%smc    = this%soil_moisture
  noah%model%slc    = this%soil_liquid
  noah%model%tsurf  = noah%model%tskin
  
  end subroutine TransferInitialNoah

  subroutine TransferInitialNoahMP(this, namelist, noahmp)
  
  use ufsLandNoahMPType
  
  class(initial_type)  :: this
  type(namelist_type)  :: namelist
  type(noahmp_type)    :: noahmp
  
  if(namelist%lensub /= noahmp%static%im) stop "vector length mismatch in ufsLandInitial_TransferInitial"
  if(this%nlevels    /= noahmp%static%km) stop "  soil levels mismatch in ufsLandInitial_TransferInitial"
  
  noahmp%model%weasd  = this%snow_water_equivalent
  noahmp%model%snwdph = this%snow_depth * 1000.0  ! driver wants mm
  noahmp%model%canopy = this%canopy_water
  noahmp%model%tskin  = this%skin_temperature
  noahmp%model%stc    = this%soil_temperature
  noahmp%model%smc    = this%soil_moisture
  noahmp%model%slc    = this%soil_liquid
  noahmp%model%tsurf  = noahmp%model%tskin
  
  end subroutine TransferInitialNoahMP

end module ufsLandInitialModule
