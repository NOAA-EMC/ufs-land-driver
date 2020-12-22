module ufsLandForcingModule

use NamelistRead

implicit none
save
private

type, public :: forcing_type

  integer                          :: forcing_counter
  integer                          :: nlocations
  double precision                 :: time
  real, allocatable, dimension(:)  :: temperature
  real, allocatable, dimension(:)  :: specific_humidity
  real, allocatable, dimension(:)  :: surface_pressure
  real, allocatable, dimension(:)  :: wind_speed
  real, allocatable, dimension(:)  :: downward_longwave
  real, allocatable, dimension(:)  :: downward_shortwave
  real, allocatable, dimension(:)  :: precipitation
  real                             :: forcing_height

  contains

    procedure, public  :: ReadForcingInit
    procedure, public  :: ReadForcing

end type forcing_type
     
contains   

  subroutine ReadForcingInit(this, namelist)
  
  use netcdf
  use error_handling, only : handle_err
  
  class(forcing_type)  :: this
  type(namelist_type)  :: namelist
  
  character*128        :: forcing_filename
  
  double precision     :: read_time
  
  integer :: ncid, dimid, varid, status, ntimes, itime
  
  this%forcing_counter = 0
  
  forcing_filename = trim(namelist%forcing_dir)//"/"//"ufs_land_forcing.19980101060000.nc"
  
  status = nf90_open(forcing_filename, NF90_NOWRITE, ncid)
   if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_dimid(ncid, "location", dimid)
   if (status /= nf90_noerr) call handle_err(status)

  status = nf90_inquire_dimension(ncid, dimid, len = this%nlocations)
   if (status /= nf90_noerr) call handle_err(status)
   
  allocate(this%temperature       (this%nlocations))
  allocate(this%specific_humidity (this%nlocations))
  allocate(this%surface_pressure  (this%nlocations))
  allocate(this%wind_speed        (this%nlocations))
  allocate(this%downward_longwave (this%nlocations))
  allocate(this%downward_shortwave(this%nlocations))
  allocate(this%precipitation     (this%nlocations))

  status = nf90_inq_dimid(ncid, "time", dimid)
   if (status /= nf90_noerr) call handle_err(status)

  status = nf90_inquire_dimension(ncid, dimid, len = ntimes)
   if (status /= nf90_noerr) call handle_err(status)
   
  status = nf90_inq_varid(ncid, "time", varid)
   if(status /= nf90_noerr) call handle_err(status)

  timeloop : do itime = 1, ntimes
  
    status = nf90_get_var(ncid, varid, read_time, start = (/itime/))
     if(status /= nf90_noerr) call handle_err(status)
     
    if (read_time == namelist%initial_time + namelist%timestep_seconds) then
       this%forcing_counter = itime
       exit timeloop
    end if
    
  end do timeloop
  
  if(this%forcing_counter == 0) stop "did not find initial forcing time in file"
   
  end subroutine ReadForcingInit

  subroutine ReadForcing(this, namelist, now_time)
  
  use netcdf
  use error_handling, only : handle_err
  
  class(forcing_type)  :: this
  type(namelist_type)  :: namelist
  double precision     :: now_time
  
  character*128        :: forcing_filename
  
  integer :: ncid, dimid, varid, status
  
  forcing_filename = trim(namelist%forcing_dir)//"/"//"ufs_land_forcing.19980101060000.nc"
  
  status = nf90_open(forcing_filename, NF90_NOWRITE, ncid)
   if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "time", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%time, start = (/this%forcing_counter/))
   if(status /= nf90_noerr) call handle_err(status)
  
  if(now_time /= this%time) then
   print *, "Read of forcing time is not consistent with model time"
   stop
  end if

  status = nf90_inq_varid(ncid, "temperature", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%temperature, start = (/1,this%forcing_counter/), count = (/this%nlocations, 1/))
   if(status /= nf90_noerr) call handle_err(status)
   
  status = nf90_inq_varid(ncid, "specific_humidity", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%specific_humidity, start = (/1,this%forcing_counter/), count = (/this%nlocations, 1/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "surface_pressure", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%surface_pressure, start = (/1,this%forcing_counter/), count = (/this%nlocations, 1/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "wind_speed", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%wind_speed, start = (/1,this%forcing_counter/), count = (/this%nlocations, 1/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "downward_longwave", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%downward_longwave, start = (/1,this%forcing_counter/), count = (/this%nlocations, 1/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_inq_varid(ncid, "downward_shortwave", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%downward_shortwave, start = (/1,this%forcing_counter/), count = (/this%nlocations, 1/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "precipitation", varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%precipitation, start = (/1,this%forcing_counter/), count = (/this%nlocations, 1/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_close(ncid)

  this%forcing_counter = this%forcing_counter + 1   ! increment forcing counter by 1

  this%wind_speed     = max(this%wind_speed, 0.1)
  this%precipitation  = this%precipitation * namelist%timestep_seconds / 1000.0 ! convert mm/s to m
  
  end subroutine ReadForcing

end module ufsLandForcingModule
