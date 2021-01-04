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
  
  forcing_type : select case (trim(namelist%forcing_type))
    case ("single_point")
      forcing_filename = trim(namelist%forcing_dir)//"/"//trim(namelist%forcing_filename)
    case ("gswp3")
      forcing_filename = trim(namelist%forcing_dir)//"/"//trim(namelist%forcing_filename)
      forcing_filename = trim(forcing_filename)//namelist%simulation_start(1:7)//".nc"
    case default
      stop "namelist forcing_type not recognized"
  end select forcing_type
  
  write(*,*) "Starting first read: "//trim(forcing_filename)
  
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
     
    if (read_time == namelist%initial_time + namelist%forcing_timestep_seconds) then
       this%forcing_counter = itime
       exit timeloop
    end if
    
  end do timeloop
  
  status = nf90_close(ncid)
   if (status /= nf90_noerr) call handle_err(status)

  if(this%forcing_counter == 0) stop "did not find initial forcing time in file"
   
  end subroutine ReadForcingInit

  subroutine ReadForcing(this, namelist, now_time)
  
  use netcdf
  use error_handling, only : handle_err
  use time_utilities
  
  class(forcing_type)  :: this
  type(namelist_type)  :: namelist
  double precision     :: now_time
  
  character*128        :: forcing_filename
  character*19         :: now_date  ! format: yyyy-mm-dd hh:nn:ss
  
  integer :: ncid, dimid, varid, status
  
  call date_from_since("1970-01-01 00:00:00", now_time, now_date)
  
  write(*,*) "Reading forcing at time: ",now_date
  
  forcing_type : select case (trim(namelist%forcing_type))
    case ("single_point")
      forcing_filename = trim(namelist%forcing_dir)//"/"//trim(namelist%forcing_filename)
    case ("gswp3")
      forcing_filename = trim(namelist%forcing_dir)//"/"//trim(namelist%forcing_filename)
      forcing_filename = trim(forcing_filename)//now_date(1:7)//".nc"
    case default
      stop "namelist forcing_type not recognized"
  end select forcing_type
  
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

  status = nf90_inq_varid(ncid, trim(namelist%forcing_name_temperature), varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%temperature, start = (/1,this%forcing_counter/), count = (/this%nlocations, 1/))
   if(status /= nf90_noerr) call handle_err(status)
   
  status = nf90_inq_varid(ncid, trim(namelist%forcing_name_specific_humidity), varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%specific_humidity, start = (/1,this%forcing_counter/), count = (/this%nlocations, 1/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, trim(namelist%forcing_name_pressure), varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%surface_pressure, start = (/1,this%forcing_counter/), count = (/this%nlocations, 1/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, trim(namelist%forcing_name_wind_speed), varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%wind_speed, start = (/1,this%forcing_counter/), count = (/this%nlocations, 1/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, trim(namelist%forcing_name_lw_radiation), varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%downward_longwave, start = (/1,this%forcing_counter/), count = (/this%nlocations, 1/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_inq_varid(ncid, trim(namelist%forcing_name_sw_radiation), varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%downward_shortwave, start = (/1,this%forcing_counter/), count = (/this%nlocations, 1/))
   if(status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, trim(namelist%forcing_name_precipitation), varid)
   if(status /= nf90_noerr) call handle_err(status)
  status = nf90_get_var(ncid, varid, this%precipitation, start = (/1,this%forcing_counter/), count = (/this%nlocations, 1/))
   if(status /= nf90_noerr) call handle_err(status)

  status = nf90_close(ncid)

  this%forcing_counter = this%forcing_counter + 1   ! increment forcing counter by 1

  this%wind_speed     = max(this%wind_speed, 0.1)
  this%precipitation  = this%precipitation * namelist%timestep_seconds / 1000.0 ! convert mm/s to m
  
  end subroutine ReadForcing

end module ufsLandForcingModule
