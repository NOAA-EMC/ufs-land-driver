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
   
  status = nf90_inq_dimid(ncid, "soil_levels", dimid)
   if (status /= nf90_noerr) call handle_err(status)

  status = nf90_inquire_dimension(ncid, dimid, len = this%nlevels)
   if (status /= nf90_noerr) call handle_err(status)
   
  allocate(this%latitude             (this%nlocations))
  allocate(this%longitude            (this%nlocations))
  allocate(this%snow_water_equivalent(this%nlocations))
  allocate(this%snow_depth           (this%nlocations))
  allocate(this%canopy_water         (this%nlocations))
  allocate(this%skin_temperature     (this%nlocations))
  allocate(this%soil_level_thickness (this%nlevels))
  allocate(this%soil_level_nodes     (this%nlevels))
  allocate(this%soil_temperature     (this%nlocations,this%nlevels))
  allocate(this%soil_moisture        (this%nlocations,this%nlevels))
  allocate(this%soil_liquid          (this%nlocations,this%nlevels))
  
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
  status = nf90_get_var(ncid, varid, this%latitude)
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "longitude", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%longitude)
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "snow_water_equivalent", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%snow_water_equivalent)
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "snow_depth", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%snow_depth)
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "canopy_water", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%canopy_water)
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_inq_varid(ncid, "skin_temperature", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%skin_temperature)
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
  status = nf90_get_var(ncid, varid, this%soil_temperature, count = (/this%nlocations, this%nlevels/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_inq_varid(ncid, "soil_moisture", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%soil_moisture, count = (/this%nlocations, this%nlevels/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_inq_varid(ncid, "soil_liquid", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%soil_liquid, count = (/this%nlocations, this%nlevels/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_get_att(ncid, NF90_GLOBAL, "iswater", this%iswater)
  status = nf90_get_att(ncid, NF90_GLOBAL, "isice"  , this%isice)
  status = nf90_get_att(ncid, NF90_GLOBAL, "isurban", this%isurban)
  status = nf90_get_att(ncid, NF90_GLOBAL, "land_cover_source", this%land_cover_source)
  
  namelist%initial_time = this%time
  
  end subroutine ReadInitial

  subroutine TransferInitialNoah(this, noah)
  
  use ufsLandNoahType
  
  class(initial_type)  :: this
  type(noah_type)      :: noah
  
  if(this%nlocations /= noah%static%im) stop "vector length mismatch in ufsLandInitial_TransferInitial"
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

  subroutine TransferInitialNoahMP(this, noahmp)
  
  use ufsLandNoahMPType
  
  class(initial_type)  :: this
  type(noahmp_type)    :: noahmp
  
  if(this%nlocations /= noahmp%static%im) stop "vector length mismatch in ufsLandInitial_TransferInitial"
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
