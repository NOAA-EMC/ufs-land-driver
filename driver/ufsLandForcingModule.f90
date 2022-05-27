module ufsLandForcingModule

use NamelistRead

implicit none
save
private

type, public :: forcing_type

  integer                          :: forcing_counter      ! current time index in netcdf forcing file
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

  double precision                 :: last_time
  character*19                     :: last_date  ! format: yyyy-mm-dd hh:nn:ss
  logical                          :: last_forcing_done
  real, allocatable, dimension(:)  :: last_temperature
  real, allocatable, dimension(:)  :: last_specific_humidity
  real, allocatable, dimension(:)  :: last_surface_pressure
  real, allocatable, dimension(:)  :: last_wind_speed
  real, allocatable, dimension(:)  :: last_downward_longwave
  real, allocatable, dimension(:)  :: last_downward_shortwave
  real, allocatable, dimension(:)  :: last_precipitation
     
  double precision                 :: next_time
  character*19                     :: next_date  ! format: yyyy-mm-dd hh:nn:ss
  logical                          :: next_forcing_done
  real, allocatable, dimension(:)  :: next_temperature
  real, allocatable, dimension(:)  :: next_specific_humidity
  real, allocatable, dimension(:)  :: next_surface_pressure
  real, allocatable, dimension(:)  :: next_wind_speed
  real, allocatable, dimension(:)  :: next_downward_longwave
  real, allocatable, dimension(:)  :: next_downward_shortwave
  real, allocatable, dimension(:)  :: next_precipitation
     
  integer                                       :: nweights
  double precision, allocatable, dimension(:)   :: regrid_weights
  integer         , allocatable, dimension(:)   :: source_lookup
  integer         , allocatable, dimension(:)   :: destination_lookup
  integer                                       :: nlats
  integer                                       :: nlons
  real            , allocatable, dimension(:,:) :: in2d_temperature
  real            , allocatable, dimension(:,:) :: in2d_specific_humidity
  real            , allocatable, dimension(:,:) :: in2d_surface_pressure
  real            , allocatable, dimension(:,:) :: in2d_wind_speed
  real            , allocatable, dimension(:,:) :: in2d_downward_longwave
  real            , allocatable, dimension(:,:) :: in2d_downward_shortwave
  real            , allocatable, dimension(:,:) :: in2d_precipitation
  real            , allocatable, dimension(:)   :: elevation_difference

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
  
! Different forcing types
!    single_point assumes all the time are in one file
!    mm_3h assumes three-hourly stored in monthly file
!    mm_1h assumes hourly stored in monthly file

  forcing_type_option : select case (trim(namelist%forcing_type))
    case ("single_point")
      forcing_filename = trim(namelist%forcing_dir)//"/"//trim(namelist%forcing_filename)
    case ("mm_3h")
      forcing_filename = trim(namelist%forcing_dir)//"/"//trim(namelist%forcing_filename)
      forcing_filename = trim(forcing_filename)//namelist%simulation_start(1:7)//".nc"
    case ("mm_1h")
      forcing_filename = trim(namelist%forcing_dir)//"/"//trim(namelist%forcing_filename)
      forcing_filename = trim(forcing_filename)//namelist%simulation_start(1:10)//".nc"
    case default
      stop "namelist forcing_type not recognized"
  end select forcing_type_option
  
  if(namelist%forcing_regrid == "esmf") then
  
    status = nf90_open(trim(namelist%forcing_regrid_weights_filename), NF90_NOWRITE, ncid)
     if (status /= nf90_noerr) call handle_err(status)
  
    status = nf90_inq_dimid(ncid, "n_s", dimid)
     if (status /= nf90_noerr) call handle_err(status)

    status = nf90_inquire_dimension(ncid, dimid, len = nweights)
     if (status /= nf90_noerr) call handle_err(status)
   
    allocate(regrid_weights(nweights))
    allocate(source_lookup(nweights))
    allocate(destination_lookup(nweights))
    
    status = nf90_inq_varid(ncid, "regrid_weights", varid)
     if(status /= nf90_noerr) call handle_err(status)

    status = nf90_get_var(ncid, varid, regrid_weights)
     if(status /= nf90_noerr) call handle_err(status)
     
    status = nf90_inq_varid(ncid, "source_lookup", varid)
     if(status /= nf90_noerr) call handle_err(status)

    status = nf90_get_var(ncid, varid, source_lookup)
     if(status /= nf90_noerr) call handle_err(status)
     
    status = nf90_inq_varid(ncid, "destination_lookup", varid)
     if(status /= nf90_noerr) call handle_err(status)

    status = nf90_get_var(ncid, varid, destination_lookup)
     if(status /= nf90_noerr) call handle_err(status)
     
    this%nlocations = maxval(destination_lookup)

    allocate(elevation_difference(this%nlocations))

    status = nf90_inq_varid(ncid, "elevation_difference", varid)
     if(status /= nf90_noerr) call handle_err(status)
    status = nf90_get_var(ncid, varid, elevation_difference, &
        start = (/namelist%subset_start/), count = (/namelist%subset_length/))
     if(status /= nf90_noerr) call handle_err(status)

    status = nf90_close(ncid)
     if (status /= nf90_noerr) call handle_err(status)
    
    write(*,*) "Read weights and elevation adjustment from: "//trim(namelist%forcing_regrid_weights_filename)

  end if
  
  write(*,*) "Starting first read: "//trim(forcing_filename)
  
  status = nf90_open(forcing_filename, NF90_NOWRITE, ncid)
   if (status /= nf90_noerr) call handle_err(status)
  
  if(namelist%forcing_regrid == "none") then
  
    status = nf90_inq_dimid(ncid, "location", dimid)
     if (status /= nf90_noerr) call handle_err(status)

    status = nf90_inquire_dimension(ncid, dimid, len = this%nlocations)
     if (status /= nf90_noerr) call handle_err(status)
   
  end if
  
  if(namelist%location_start > this%nlocations .or. namelist%location_end > this%nlocations) &
    stop "location_start or location_end in namelist not consistent with nlocations in forcing read"
   
  allocate(this%temperature       (namelist%subset_length))
  allocate(this%specific_humidity (namelist%subset_length))
  allocate(this%surface_pressure  (namelist%subset_length))
  allocate(this%wind_speed        (namelist%subset_length))
  allocate(this%downward_longwave (namelist%subset_length))
  allocate(this%downward_shortwave(namelist%subset_length))
  allocate(this%precipitation     (namelist%subset_length))

  allocate(last_temperature       (namelist%subset_length))
  allocate(last_specific_humidity (namelist%subset_length))
  allocate(last_surface_pressure  (namelist%subset_length))
  allocate(last_wind_speed        (namelist%subset_length))
  allocate(last_downward_longwave (namelist%subset_length))
  allocate(last_downward_shortwave(namelist%subset_length))
  allocate(last_precipitation     (namelist%subset_length))

  allocate(next_temperature       (namelist%subset_length))
  allocate(next_specific_humidity (namelist%subset_length))
  allocate(next_surface_pressure  (namelist%subset_length))
  allocate(next_wind_speed        (namelist%subset_length))
  allocate(next_downward_longwave (namelist%subset_length))
  allocate(next_downward_shortwave(namelist%subset_length))
  allocate(next_precipitation     (namelist%subset_length))

  status = nf90_inq_dimid(ncid, "time", dimid)
   if (status /= nf90_noerr) call handle_err(status)

  status = nf90_inquire_dimension(ncid, dimid, len = ntimes)
   if (status /= nf90_noerr) call handle_err(status)
   
  status = nf90_inq_varid(ncid, "time", varid)
   if(status /= nf90_noerr) call handle_err(status)

! loop through times of the forcing file to find the index to start reading

  timeloop : do itime = 1, ntimes
  
    status = nf90_get_var(ncid, varid, read_time, start = (/itime/))
     if(status /= nf90_noerr) call handle_err(status)
     
    if (read_time == namelist%initial_time + namelist%timestep_seconds) then
       this%forcing_counter = itime
       next_time = read_time
       last_time = huge(1.d0)
       next_forcing_done = .false.
       last_forcing_done = .false.
       exit timeloop
    end if
    
  end do timeloop
  
  status = nf90_close(ncid)
   if (status /= nf90_noerr) call handle_err(status)

  if(this%forcing_counter == 0) stop "did not find initial forcing time in file"
   
  end subroutine ReadForcingInit

  subroutine ReadForcing(this, namelist, static, now_time)
  
  use netcdf
  use error_handling, only : handle_err
  use time_utilities
  use interpolation_utilities, only : interpolate_linear, interpolate_zenith
  use ufsLandStaticModule, only : static_type
  
  class(forcing_type)  :: this
  type(namelist_type)  :: namelist
  type (static_type)   :: static
  double precision     :: now_time
  
  character*128        :: forcing_filename
  character*19         :: now_date  ! format: yyyy-mm-dd hh:nn:ss
  
  integer :: ncid, dimid, varid, status
  integer :: times_in_file
  integer :: iloc, latloc, lonloc
  double precision  :: file_next_time
  
  call date_from_since("1970-01-01 00:00:00", now_time, now_date)
  call date_from_since("1970-01-01 00:00:00", next_time, next_date)
  
  write(*,*) "Searching for forcing at time: ",now_date
  
  if(.not. next_forcing_done) then
  
    forcing_type_option : select case (trim(namelist%forcing_type))
      case ("single_point")
        forcing_filename = trim(namelist%forcing_dir)//"/"//trim(namelist%forcing_filename)
      case ("mm_3h")
        forcing_filename = trim(namelist%forcing_dir)//"/"//trim(namelist%forcing_filename)
        forcing_filename = trim(forcing_filename)//next_date(1:7)//".nc"
        if(next_date(9:19) == "01 00:00:00") then
          this%forcing_counter = 1
          write(*,*) "Resetting forcing counter to beginning of file"
        end if
      case ("mm_1h")
        forcing_filename = trim(namelist%forcing_dir)//"/"//trim(namelist%forcing_filename)
        forcing_filename = trim(forcing_filename)//next_date(1:10)//".nc"
        if(next_date(12:19) == "00:00:00") then
          this%forcing_counter = 1
          write(*,*) "Resetting forcing counter to beginning of file"
        end if
      case default
        stop "namelist forcing_type not recognized"
    end select forcing_type_option
  
    status = nf90_open(forcing_filename, NF90_NOWRITE, ncid)
     if (status /= nf90_noerr) call handle_err(status)
  
    status = nf90_inq_varid(ncid, "time", varid)
     if(status /= nf90_noerr) call handle_err(status)
    status = nf90_get_var(ncid, varid, file_next_time, start = (/this%forcing_counter/))
     if(status /= nf90_noerr) call handle_err(status)
     
    if(file_next_time /= next_time) then
      write(*,*) "file_next_time not equal to next_time in now_time == next_time forcing"
      stop
    end if
   
    if(namelist%forcing_regrid == "none") then
   
      status = nf90_inq_varid(ncid, trim(namelist%forcing_name_temperature), varid)
       if(status /= nf90_noerr) call handle_err(status)
      status = nf90_get_var(ncid, varid, next_temperature, &
          start = (/namelist%subset_start,this%forcing_counter/), count = (/namelist%subset_length, 1/))
       if(status /= nf90_noerr) call handle_err(status)
  
      status = nf90_inq_varid(ncid, trim(namelist%forcing_name_specific_humidity), varid)
       if(status /= nf90_noerr) call handle_err(status)
      status = nf90_get_var(ncid, varid, next_specific_humidity, &
          start = (/namelist%subset_start,this%forcing_counter/), count = (/namelist%subset_length, 1/))
       if(status /= nf90_noerr) call handle_err(status)
  
      status = nf90_inq_varid(ncid, trim(namelist%forcing_name_pressure), varid)
       if(status /= nf90_noerr) call handle_err(status)
      status = nf90_get_var(ncid, varid, next_surface_pressure, &
          start = (/namelist%subset_start,this%forcing_counter/), count = (/namelist%subset_length, 1/))
       if(status /= nf90_noerr) call handle_err(status)
  
      status = nf90_inq_varid(ncid, trim(namelist%forcing_name_wind_speed), varid)
       if(status /= nf90_noerr) call handle_err(status)
      status = nf90_get_var(ncid, varid, next_wind_speed, &
          start = (/namelist%subset_start,this%forcing_counter/), count = (/namelist%subset_length, 1/))
       if(status /= nf90_noerr) call handle_err(status)
  
      status = nf90_inq_varid(ncid, trim(namelist%forcing_name_lw_radiation), varid)
       if(status /= nf90_noerr) call handle_err(status)
      status = nf90_get_var(ncid, varid, next_downward_longwave, &
          start = (/namelist%subset_start,this%forcing_counter/), count = (/namelist%subset_length, 1/))
       if(status /= nf90_noerr) call handle_err(status)

      status = nf90_inq_varid(ncid, trim(namelist%forcing_name_sw_radiation), varid)
       if(status /= nf90_noerr) call handle_err(status)
      status = nf90_get_var(ncid, varid, next_downward_shortwave, &
          start = (/namelist%subset_start,this%forcing_counter/), count = (/namelist%subset_length, 1/))
       if(status /= nf90_noerr) call handle_err(status)
  
      status = nf90_inq_varid(ncid, trim(namelist%forcing_name_precipitation), varid)
       if(status /= nf90_noerr) call handle_err(status)
      status = nf90_get_var(ncid, varid, next_precipitation, &
          start = (/namelist%subset_start,this%forcing_counter/), count = (/namelist%subset_length, 1/))
       if(status /= nf90_noerr) call handle_err(status)

    elseif(namelist%forcing_regrid == "esmf") then
   
      status = nf90_inq_dimid(ncid, "latitude", dimid)
       if (status /= nf90_noerr) call handle_err(status)

      status = nf90_inquire_dimension(ncid, dimid, len = nlats)
       if (status /= nf90_noerr) call handle_err(status)
   
      status = nf90_inq_dimid(ncid, "longitude", dimid)
       if (status /= nf90_noerr) call handle_err(status)

      status = nf90_inquire_dimension(ncid, dimid, len = nlons)
       if (status /= nf90_noerr) call handle_err(status)
   
      allocate(in2d_temperature       (nlons,nlats))
      allocate(in2d_specific_humidity (nlons,nlats))
      allocate(in2d_surface_pressure  (nlons,nlats))
      allocate(in2d_wind_speed        (nlons,nlats))
      allocate(in2d_downward_longwave (nlons,nlats))
      allocate(in2d_downward_shortwave(nlons,nlats))
      allocate(in2d_precipitation     (nlons,nlats))
      
      status = nf90_inq_varid(ncid, trim(namelist%forcing_name_temperature), varid)
       if(status /= nf90_noerr) call handle_err(status)
      status = nf90_get_var(ncid, varid, in2d_temperature, &
          start = (/1, 1, this%forcing_counter/), count = (/nlons, nlats, 1/))
       if(status /= nf90_noerr) call handle_err(status)
  
      status = nf90_inq_varid(ncid, trim(namelist%forcing_name_specific_humidity), varid)
       if(status /= nf90_noerr) call handle_err(status)
      status = nf90_get_var(ncid, varid, in2d_specific_humidity, &
          start = (/1, 1, this%forcing_counter/), count = (/nlons, nlats, 1/))
       if(status /= nf90_noerr) call handle_err(status)
  
      status = nf90_inq_varid(ncid, trim(namelist%forcing_name_pressure), varid)
       if(status /= nf90_noerr) call handle_err(status)
      status = nf90_get_var(ncid, varid, in2d_surface_pressure, &
          start = (/1, 1, this%forcing_counter/), count = (/nlons, nlats, 1/))
       if(status /= nf90_noerr) call handle_err(status)
  
      status = nf90_inq_varid(ncid, trim(namelist%forcing_name_wind_speed), varid)
       if(status /= nf90_noerr) call handle_err(status)
      status = nf90_get_var(ncid, varid, in2d_wind_speed, &
          start = (/1, 1, this%forcing_counter/), count = (/nlons, nlats, 1/))
       if(status /= nf90_noerr) call handle_err(status)
  
      status = nf90_inq_varid(ncid, trim(namelist%forcing_name_lw_radiation), varid)
       if(status /= nf90_noerr) call handle_err(status)
      status = nf90_get_var(ncid, varid, in2d_downward_longwave, &
          start = (/1, 1, this%forcing_counter/), count = (/nlons, nlats, 1/))
       if(status /= nf90_noerr) call handle_err(status)

      status = nf90_inq_varid(ncid, trim(namelist%forcing_name_sw_radiation), varid)
       if(status /= nf90_noerr) call handle_err(status)
      status = nf90_get_var(ncid, varid, in2d_downward_shortwave, &
          start = (/1, 1, this%forcing_counter/), count = (/nlons, nlats, 1/))
       if(status /= nf90_noerr) call handle_err(status)
  
      status = nf90_inq_varid(ncid, trim(namelist%forcing_name_precipitation), varid)
       if(status /= nf90_noerr) call handle_err(status)
      status = nf90_get_var(ncid, varid, in2d_precipitation, &
          start = (/1, 1, this%forcing_counter/), count = (/nlons, nlats, 1/))
       if(status /= nf90_noerr) call handle_err(status)

      next_temperature = 0.0
      next_specific_humidity = 0.0
      next_surface_pressure = 0.0
      next_wind_speed = 0.0
      next_downward_longwave = 0.0
      next_downward_shortwave = 0.0
      next_precipitation = 0.0

      do iloc = 1, nweights

        latloc = source_lookup(iloc)/nlons + 1
        lonloc = source_lookup(iloc) - (latloc-1)*nlons

        next_temperature(destination_lookup(iloc)) = next_temperature(destination_lookup(iloc)) + &
                                     regrid_weights(iloc) * in2d_temperature(lonloc,latloc) 
        next_specific_humidity(destination_lookup(iloc)) = next_specific_humidity(destination_lookup(iloc)) + &
                                     regrid_weights(iloc) * in2d_specific_humidity(lonloc,latloc)
        next_surface_pressure(destination_lookup(iloc)) = next_surface_pressure(destination_lookup(iloc)) + &
                                     regrid_weights(iloc) * in2d_surface_pressure(lonloc,latloc)
        next_wind_speed(destination_lookup(iloc)) = next_wind_speed(destination_lookup(iloc)) + &
                                     regrid_weights(iloc) * in2d_wind_speed(lonloc,latloc)
        next_downward_longwave(destination_lookup(iloc)) = next_downward_longwave(destination_lookup(iloc)) + &
                                     regrid_weights(iloc) * in2d_downward_longwave(lonloc,latloc)
        next_downward_shortwave(destination_lookup(iloc)) = next_downward_shortwave(destination_lookup(iloc)) + &
                                     regrid_weights(iloc) * in2d_downward_shortwave(lonloc,latloc)
        next_precipitation(destination_lookup(iloc)) = next_precipitation(destination_lookup(iloc)) + &
                                     regrid_weights(iloc) * in2d_precipitation(lonloc,latloc)

      end do

! adjust for elevation difference between input forcing and model grid
! cap elevation difference at 2000 meters, arbitrarily, to be consistent with pre-process regrid method
! need to explore need to do this

      where(elevation_difference >  2000.0) elevation_difference =  2000.0
      where(elevation_difference < -2000.0) elevation_difference = -2000.0
      next_temperature = next_temperature + elevation_difference * 0.0065

      deallocate(in2d_temperature       )
      deallocate(in2d_specific_humidity )
      deallocate(in2d_surface_pressure  )
      deallocate(in2d_wind_speed        )
      deallocate(in2d_downward_longwave )
      deallocate(in2d_downward_shortwave)
      deallocate(in2d_precipitation     )

    else

      write(*,*) "unknown forcing regrid option"
      stop
    
    end if
   
    status = nf90_close(ncid)

    next_forcing_done = .true.
    
  end if ! not read_next_forcing
  
  if(now_time == next_time) then  ! transfer the next forcing just read into the current forcing 

    this%temperature        = next_temperature
    this%specific_humidity  = next_specific_humidity
    this%surface_pressure   = next_surface_pressure
    this%wind_speed         = next_wind_speed
    this%downward_longwave  = next_downward_longwave
    this%precipitation      = next_precipitation
    
    if(namelist%forcing_time_solar == "instantaneous") then
      this%downward_shortwave = next_downward_shortwave
    elseif(namelist%forcing_time_solar == "period_average") then
      call interpolate_zenith(now_time, next_time, namelist%subset_length,             &
                              static%latitude, static%longitude,                &
			      namelist%timestep_seconds,                        &
                              next_downward_shortwave,                          &
                              this%downward_shortwave)
    else
      write(*,*) namelist%forcing_time_solar, " namelist%forcing_time_solar not recognized"
      stop
    end if
    
! since there is no future forcing in memory, prepare for next forcing read
    
    last_time               = next_time
    last_temperature        = next_temperature
    last_specific_humidity  = next_specific_humidity
    last_surface_pressure   = next_surface_pressure
    last_wind_speed         = next_wind_speed
    last_downward_longwave  = next_downward_longwave
    last_downward_shortwave = next_downward_shortwave
    last_precipitation      = next_precipitation
    
    next_time = last_time + namelist%forcing_timestep_seconds   ! increment next_time to next forcing time
    last_forcing_done = .true.                                  ! last_time forcing is in memory
    next_forcing_done = .false.                                 ! next_time forcing is not in memory
  
    this%forcing_counter = this%forcing_counter + 1             ! increment forcing counter by 1
    
  elseif(now_time < next_time) then ! between forcing times
  
    call interpolate_linear(now_time, last_time, next_time, namelist%subset_length, &
                            last_temperature, next_temperature, this%temperature)
  
    call interpolate_linear(now_time, last_time, next_time, namelist%subset_length, &
                            last_specific_humidity, next_specific_humidity, this%specific_humidity)

    call interpolate_linear(now_time, last_time, next_time, namelist%subset_length, &
                            last_surface_pressure, next_surface_pressure, this%surface_pressure)

    call interpolate_linear(now_time, last_time, next_time, namelist%subset_length, &
                            last_wind_speed, next_wind_speed, this%wind_speed)

    call interpolate_linear(now_time, last_time, next_time, namelist%subset_length, &
                            last_downward_longwave, next_downward_longwave, this%downward_longwave)

    if(trim(namelist%forcing_interp_solar) == "linear") then

      call interpolate_linear(now_time, last_time, next_time, namelist%subset_length, &
                              last_downward_shortwave, next_downward_shortwave, this%downward_shortwave)

    elseif(trim(namelist%forcing_interp_solar) == "zenith") then

      call interpolate_zenith(now_time, last_time, namelist%subset_length,      &
                              static%latitude, static%longitude,                &
			      namelist%timestep_seconds,                        &
                              last_downward_shortwave,                          &
                              this%downward_shortwave)
    else
      write(*,*) namelist%forcing_time_solar, " namelist%forcing_time_solar not recognized"
      stop
    end if

    this%precipitation = next_precipitation
  
    next_forcing_done = .true.

  else

    write(*,*) "Read of forcing time is not consistent with model time"
    stop

  end if

! do a gross check on wind speed and convert forcing precipitation units
    
  this%wind_speed     = max(this%wind_speed, 0.1)
  this%precipitation  = this%precipitation * namelist%timestep_seconds / 1000.0 ! convert mm/s to m
  
  end subroutine ReadForcing

end module ufsLandForcingModule
