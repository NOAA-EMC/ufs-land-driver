module ufsLandNoahMPRestartModule

  implicit none
  save
  private

  type, public :: noahmp_restart_type

    character*256    :: filename

  contains

    procedure, public  :: WriteRestartNoahMP
    procedure, public  :: ReadRestartNoahMP

end type noahmp_restart_type
     
contains   

  subroutine WriteRestartNoahMP(this, namelist, noahmp, now_time)
  
  use mpi
  use netcdf
  use time_utilities
  use error_handling, only : handle_err
  use NamelistRead
  use ufsLandNoahMPType
  use ufsLandNetcdf
  use ufsLandGenericIO

  class(noahmp_restart_type)   :: this  
  type(namelist_type)  :: namelist
  type(noahmp_type)    :: noahmp
  double precision     :: now_time
  character*19     :: nowdate    ! current date
  integer          :: yyyy,mm,dd,hh,nn,ss
  integer :: ncid, dimid, varid, status
  integer :: dim_id_time, dim_id_loc, dim_id_soil, dim_id_snow, dim_id_snso, dim_id_date, dim_id_rad

  integer, parameter :: output = 1, restart = 2

  call date_from_since(namelist%reference_date, now_time, nowdate)
  read(nowdate( 1: 4),'(i4.4)') yyyy
  read(nowdate( 6: 7),'(i2.2)') mm
  read(nowdate( 9:10),'(i2.2)') dd
  read(nowdate(12:13),'(i2.2)') hh
  read(nowdate(15:16),'(i2.2)') nn
  read(nowdate(18:19),'(i2.2)') ss

  write(this%filename,'(a17,i4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a3)') &
    "ufs_land_restart.", yyyy, "-", mm, "-", dd, "_", hh, "-", nn, "-", ss, ".nc"

  this%filename = trim(namelist%restart_dir)//"/"//trim(this%filename)

  write(*,*) "Creating: "//trim(this%filename)

  status = nf90_create(this%filename, NF90_NETCDF4, ncid, comm = MPI_COMM_WORLD, &
     info = MPI_INFO_NULL)
    if (status /= nf90_noerr) call handle_err(status)

! Define dimensions in the file.

  status = nf90_def_dim(ncid, "location"    , namelist%location_length      , dim_id_loc)
    if (status /= nf90_noerr) call handle_err(status)
  status = nf90_def_dim(ncid, "soil_levels" , noahmp%static%soil_levels     , dim_id_soil)
    if (status /= nf90_noerr) call handle_err(status)
  status = nf90_def_dim(ncid, "snow_levels" , 3                             , dim_id_snow)
    if (status /= nf90_noerr) call handle_err(status)
  status = nf90_def_dim(ncid, "snso_levels" , noahmp%static%soil_levels + 3 , dim_id_snso)
    if (status /= nf90_noerr) call handle_err(status)
  status = nf90_def_dim(ncid, "radiation_bands" , 2                         , dim_id_rad)
    if (status /= nf90_noerr) call handle_err(status)
  status = nf90_def_dim(ncid, "time"        , NF90_UNLIMITED                , dim_id_time)
    if (status /= nf90_noerr) call handle_err(status)
  
! Define variables in the file.

  status = nf90_def_var(ncid, "time", NF90_DOUBLE, dim_id_time, varid)
    status = nf90_put_att(ncid, varid, "long_name", "time")
    status = nf90_put_att(ncid, varid, "units", "seconds since "//namelist%reference_date)

  status = nf90_def_var(ncid, "timestep", NF90_DOUBLE, dim_id_time, varid)
    status = nf90_put_att(ncid, varid, "long_name", "time step")
    status = nf90_put_att(ncid, varid, "units", "seconds")

  call DefineNoahMP(restart, noahmp, ncid, &
                    dim_id_time, dim_id_loc, dim_id_soil, &
                    dim_id_snow, dim_id_snso, dim_id_date, dim_id_rad)

  status = nf90_enddef(ncid)

! Start writing restart file
  
  status = nf90_inq_varid(ncid, "time", varid)
  status = nf90_var_par_access(ncid, varid, NF90_COLLECTIVE)
  status = nf90_put_var(ncid, varid , now_time             )
  
  status = nf90_inq_varid(ncid, "timestep", varid)
  status = nf90_var_par_access(ncid, varid, NF90_COLLECTIVE)
  status = nf90_put_var(ncid, varid , noahmp%static%timestep   )
  
  call WriteNoahMP(restart, namelist, noahmp, ncid, 1)

  status = nf90_close(ncid)

  end subroutine WriteRestartNoahMP
  
  subroutine ReadRestartNoahMP(this, namelist, noahmp)
  
  use netcdf
  use error_handling, only : handle_err
  use time_utilities
  use NamelistRead
  use ufsLandNoahMPType
  use ufsLandNetcdf
  use ufsLandGenericIO
  
  class(noahmp_restart_type)  :: this
  type(namelist_type)  :: namelist
  type (noahmp_type)   :: noahmp
  double precision     :: now_time
  
  integer          :: yyyy,mm,dd,hh,nn,ss
  integer :: ncid, dimid, varid, status
  integer :: dim_id_time, dim_id_loc, dim_id_soil, dim_id_snow, dim_id_snso, dim_id_date
  
  call calc_sec_since(namelist%reference_date,namelist%restart_date,0,now_time)

  namelist%initial_time = now_time

  read(namelist%restart_date( 1: 4),'(i4.4)') yyyy
  read(namelist%restart_date( 6: 7),'(i2.2)') mm
  read(namelist%restart_date( 9:10),'(i2.2)') dd
  read(namelist%restart_date(12:13),'(i2.2)') hh
  read(namelist%restart_date(15:16),'(i2.2)') nn
  read(namelist%restart_date(18:19),'(i2.2)') ss

  write(this%filename,'(a17,i4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a3)') &
    "ufs_land_restart.", yyyy, "-", mm, "-", dd, "_", hh, "-", nn, "-", ss, ".nc"

  this%filename = trim(namelist%restart_dir)//"/"//trim(this%filename)

  write(*,*) "Reading: "//trim(this%filename)
    
  status = nf90_open(this%filename, NF90_NOWRITE, ncid)
   if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "time", varid)
  status = nf90_get_var(ncid, varid , now_time)
  
  call ReadNoahMP(namelist, noahmp, ncid)

  status = nf90_close(ncid)

  end subroutine ReadRestartNoahMP

end module ufsLandNoahMPRestartModule
