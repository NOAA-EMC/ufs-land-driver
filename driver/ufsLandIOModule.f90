module ufsLandIOModule

  implicit none
  save
  private

  type, public :: output_type

    character*256    :: filename
    integer          :: output_counter

  contains

    procedure, public  :: WriteOutputNoah
    procedure, public  :: WriteOutputNoahMP

end type output_type
     
contains   

  subroutine WriteOutputNoah(this, namelist, noah, forcing, now_time)
  
  use mpi
  use netcdf
  use time_utilities
  use error_handling, only : handle_err
  use NamelistRead
  use ufsLandNoahType
  use ufsLandForcingModule

  class(output_type)   :: this  
  type(namelist_type)  :: namelist
  type(noah_type)      :: noah
  type (forcing_type)  :: forcing
  double precision     :: now_time
  character*19     :: nowdate    ! current date
  integer          :: yyyy,mm,dd,hh,nn,ss
  integer :: ncid, dimid, varid, status
  integer :: dim_id_time, dim_id_loc, dim_id_soil, dim_id_date
  
  if(now_time == namelist%initial_time + namelist%timestep_seconds .or. &
     namelist%separate_output) then
  
    call date_from_since(namelist%reference_date, now_time, nowdate)
    read(nowdate( 1: 4),'(i4.4)') yyyy
    read(nowdate( 6: 7),'(i2.2)') mm
    read(nowdate( 9:10),'(i2.2)') dd
    read(nowdate(12:13),'(i2.2)') hh
    read(nowdate(15:16),'(i2.2)') nn
    read(nowdate(18:19),'(i2.2)') ss

    write(this%filename,'(a16,i4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a3)') &
      "ufs_land_output.", yyyy, "-", mm, "-", dd, "_", hh, "-", nn, "-", ss, ".nc"

    this%filename = trim(namelist%output_dir)//"/"//trim(this%filename)

    write(*,*) "Creating: "//trim(this%filename)

    status = nf90_create(this%filename, NF90_NETCDF4, ncid, comm = MPI_COMM_WORLD, &
       info = MPI_INFO_NULL)
      if (status /= nf90_noerr) call handle_err(status)

! Define dimensions in the file.

    status = nf90_def_dim(ncid, "location"   , namelist%location_length , dim_id_loc)
      if (status /= nf90_noerr) call handle_err(status)
    status = nf90_def_dim(ncid, "soil_levels", noah%static%km           , dim_id_soil)
      if (status /= nf90_noerr) call handle_err(status)
    status = nf90_def_dim(ncid, "time"       , NF90_UNLIMITED           , dim_id_time)
      if (status /= nf90_noerr) call handle_err(status)
  
! Define variables in the file.

    status = nf90_def_var(ncid, "time", NF90_DOUBLE, dim_id_time, varid)
      status = nf90_put_att(ncid, varid, "long_name", "time")
      status = nf90_put_att(ncid, varid, "units", "seconds since "//namelist%reference_date)

    status = nf90_def_var(ncid, "delt", NF90_FLOAT, (/dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "time step")
      status = nf90_put_att(ncid, varid, "units", "seconds")

    status = nf90_def_var(ncid, "ps", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface pressure")
      status = nf90_put_att(ncid, varid, "units", "Pa")

    status = nf90_def_var(ncid, "t1", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "forcing temperature")
      status = nf90_put_att(ncid, varid, "units", "K")

    status = nf90_def_var(ncid, "q1", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "forcing specific humidity")
      status = nf90_put_att(ncid, varid, "units", "kg/kg")

    status = nf90_def_var(ncid, "sigmaf", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "green vegetation fraction")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_def_var(ncid, "sfcemis", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface emissivity")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_def_var(ncid, "dlwflx", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "forcing longwave downward flux")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "dswsfc", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "forcing shortwave downward flux")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "snet", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "forcing net shortwave flux")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "tg3", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "deep soil temperature")
      status = nf90_put_att(ncid, varid, "units", "K")

    status = nf90_def_var(ncid, "cm", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface exchange coeff for momentum")
      status = nf90_put_att(ncid, varid, "units", "m/s")

    status = nf90_def_var(ncid, "ch", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface exchange coeff heat & moisture")
      status = nf90_put_att(ncid, varid, "units", "m/s")

    status = nf90_def_var(ncid, "prsl1", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "sfc layer 1 mean pressure")
      status = nf90_put_att(ncid, varid, "units", "Pa")

    status = nf90_def_var(ncid, "prslki", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "Exner function from layer 1 to sfc")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_def_var(ncid, "zf", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "height of bottom layer")
      status = nf90_put_att(ncid, varid, "units", "m")

    status = nf90_def_var(ncid, "wind", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "wind speed")
      status = nf90_put_att(ncid, varid, "units", "m/s")
 
    status = nf90_def_var(ncid, "shdmin", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "min fractional coverage of green veg")
      status = nf90_put_att(ncid, varid, "units", "fraction")

    status = nf90_def_var(ncid, "shdmax", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "max fractional coverage of green veg")
      status = nf90_put_att(ncid, varid, "units", "fraction")

    status = nf90_def_var(ncid, "snoalb", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "upper bound on max albedo over deep snow")
      status = nf90_put_att(ncid, varid, "units", "fraction")

    status = nf90_def_var(ncid, "sfalb", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "mean sfc diffuse sw albedo")
      status = nf90_put_att(ncid, varid, "units", "fraction")

    status = nf90_def_var(ncid, "weasd", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "water equivalent accumulated snow depth")
      status = nf90_put_att(ncid, varid, "units", "mm")

    status = nf90_def_var(ncid, "snwdph", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "snow depth (water equiv) over land")
      status = nf90_put_att(ncid, varid, "units", "m")

    status = nf90_def_var(ncid, "tskin", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "ground surface skin temperature")
      status = nf90_put_att(ncid, varid, "units", "K")

    status = nf90_def_var(ncid, "tprcp", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "total precipitation")
      status = nf90_put_att(ncid, varid, "units", "mm")

    status = nf90_def_var(ncid, "srflag", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "snow/rain flag for precipitation")
      status = nf90_put_att(ncid, varid, "units", "fraction")

    status = nf90_def_var(ncid, "smc", NF90_FLOAT, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "total soil moisture content")
      status = nf90_put_att(ncid, varid, "units", "m3/m3")

    status = nf90_def_var(ncid, "stc", NF90_FLOAT, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "soil temperature")
      status = nf90_put_att(ncid, varid, "units", "K")

    status = nf90_def_var(ncid, "slc", NF90_FLOAT, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "liquid soil moisture")
      status = nf90_put_att(ncid, varid, "units", "m3/m3")

    status = nf90_def_var(ncid, "canopy", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "canopy moisture content")
      status = nf90_put_att(ncid, varid, "units", "m")

    status = nf90_def_var(ncid, "trans", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "total plant transpiration")
      status = nf90_put_att(ncid, varid, "units", "m/s")

    status = nf90_def_var(ncid, "tsurf", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface skin temperature (after iteration)")
      status = nf90_put_att(ncid, varid, "units", "K")

    status = nf90_def_var(ncid, "zorl", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface roughness")
      status = nf90_put_att(ncid, varid, "units", "m")

    status = nf90_def_var(ncid, "sncovr1", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "snow cover over land")
      status = nf90_put_att(ncid, varid, "units", "fraction")

    status = nf90_def_var(ncid, "qsurf", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "specific humidity at sfc")
      status = nf90_put_att(ncid, varid, "units", "kg/kg")

    status = nf90_def_var(ncid, "gflux", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "soil heat flux")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "drain", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "subsurface runoff")
      status = nf90_put_att(ncid, varid, "units", "mm/s")

    status = nf90_def_var(ncid, "evap", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "evaporation from latent heat flux")
      status = nf90_put_att(ncid, varid, "units", "mm/s")

    status = nf90_def_var(ncid, "hflx", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "sensible heat flux")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "ep", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "potential evaporation")
      status = nf90_put_att(ncid, varid, "units", "?")

    status = nf90_def_var(ncid, "runoff", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface runoff")
      status = nf90_put_att(ncid, varid, "units", "m/s")

    status = nf90_def_var(ncid, "cmm", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "ch * rho")
      status = nf90_put_att(ncid, varid, "units", "")

    status = nf90_def_var(ncid, "chh", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "")
      status = nf90_put_att(ncid, varid, "units", "m/s")

    status = nf90_def_var(ncid, "evbs", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "direct soil evaporation")
      status = nf90_put_att(ncid, varid, "units", "m/s")

    status = nf90_def_var(ncid, "evcw", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "canopy water evaporation")
      status = nf90_put_att(ncid, varid, "units", "m/s")

    status = nf90_def_var(ncid, "sbsno", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "sublimation/deposit from snopack")
      status = nf90_put_att(ncid, varid, "units", "m/s")

    status = nf90_def_var(ncid, "snowc", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "fractional snow cover")
      status = nf90_put_att(ncid, varid, "units", "fraction")

    status = nf90_def_var(ncid, "stm", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "total soil column moisture content")
      status = nf90_put_att(ncid, varid, "units", "m")

    status = nf90_def_var(ncid, "snohf", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "snow/freezing-rain latent heat flux ")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "smcwlt2", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "dry soil moisture threshold")
      status = nf90_put_att(ncid, varid, "units", "m3/m3")

    status = nf90_def_var(ncid, "smcref2", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "soil moisture threshold")
      status = nf90_put_att(ncid, varid, "units", "m3/m3")

    status = nf90_def_var(ncid, "wet1", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "normalized soil wetness")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_enddef(ncid)

    status = nf90_close(ncid)
    
    this%output_counter = 1

  end if
    
  status = nf90_open(this%filename, NF90_WRITE, ncid, comm = MPI_COMM_WORLD, &
       info = MPI_INFO_NULL)
   if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "time", varid)
  status = nf90_put_var(ncid, varid , now_time           , start = (/this%output_counter/))
  
  status = nf90_inq_varid(ncid, "delt", varid)
  status = nf90_put_var(ncid, varid , noah%static%delt   , start = (/this%output_counter/))

  status = nf90_inq_varid(ncid, "ps", varid)
  status = nf90_put_var(ncid, varid , forcing%surface_pressure   , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "t1", varid)
  status = nf90_put_var(ncid, varid , forcing%temperature        , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "q1", varid)
  status = nf90_put_var(ncid, varid , forcing%specific_humidity  , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "wind", varid)
  status = nf90_put_var(ncid, varid , forcing%wind_speed         , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "tprcp", varid)
  status = nf90_put_var(ncid, varid , forcing%precipitation      , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "dlwflx", varid)
  status = nf90_put_var(ncid, varid , forcing%downward_longwave  , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "dswsfc", varid)
  status = nf90_put_var(ncid, varid , forcing%downward_shortwave , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "sigmaf", varid)
  status = nf90_put_var(ncid, varid , noah%model%sigmaf          , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "sfcemis", varid)
  status = nf90_put_var(ncid, varid , noah%model%sfcemis         , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "snet", varid)
  status = nf90_put_var(ncid, varid , noah%model%snet            , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "tg3", varid)
  status = nf90_put_var(ncid, varid , noah%model%tg3             , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "cm", varid)
  status = nf90_put_var(ncid, varid , noah%model%cm              , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "ch", varid)
  status = nf90_put_var(ncid, varid , noah%model%ch              , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "prsl1", varid)
  status = nf90_put_var(ncid, varid , noah%model%prsl1           , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "prslki", varid)
  status = nf90_put_var(ncid, varid , noah%model%prslki          , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "zf", varid)
  status = nf90_put_var(ncid, varid , noah%model%zf              , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "shdmin", varid)
  status = nf90_put_var(ncid, varid , noah%model%shdmin          , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "shdmax", varid)
  status = nf90_put_var(ncid, varid , noah%model%shdmax          , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "snoalb", varid)
  status = nf90_put_var(ncid, varid , noah%model%snoalb          , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "sfalb", varid)
  status = nf90_put_var(ncid, varid , noah%model%sfalb           , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "weasd", varid)
  status = nf90_put_var(ncid, varid , noah%model%weasd           , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "snwdph", varid)
  status = nf90_put_var(ncid, varid , noah%model%snwdph          , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "tskin", varid)
  status = nf90_put_var(ncid, varid , noah%model%tskin           , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "srflag", varid)
  status = nf90_put_var(ncid, varid , noah%model%srflag          , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "smc", varid)
  status = nf90_put_var(ncid, varid , noah%model%smc                , &
      start = (/namelist%subset_start ,              1,this%output_counter/), &
      count = (/namelist%subset_length, noah%static%km,                  1/))

  status = nf90_inq_varid(ncid, "stc", varid)
  status = nf90_put_var(ncid, varid , noah%model%stc                , &
      start = (/namelist%subset_start ,              1,this%output_counter/), &
      count = (/namelist%subset_length, noah%static%km,                  1/))

  status = nf90_inq_varid(ncid, "slc", varid)
  status = nf90_put_var(ncid, varid , noah%model%slc                , &
      start = (/namelist%subset_start ,              1,this%output_counter/), &
      count = (/namelist%subset_length, noah%static%km,                  1/))

  status = nf90_inq_varid(ncid, "canopy", varid)
  status = nf90_put_var(ncid, varid , noah%model%canopy          , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "trans", varid)
  status = nf90_put_var(ncid, varid , noah%model%trans           , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "tsurf", varid)
  status = nf90_put_var(ncid, varid , noah%model%tsurf           , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "zorl", varid)
  status = nf90_put_var(ncid, varid , noah%model%zorl            , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "sncovr1", varid)
  status = nf90_put_var(ncid, varid , noah%model%sncovr1         , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "qsurf", varid)
  status = nf90_put_var(ncid, varid , noah%model%qsurf           , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "gflux", varid)
  status = nf90_put_var(ncid, varid , noah%model%gflux           , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "drain", varid)
  status = nf90_put_var(ncid, varid , noah%model%drain           , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "evap", varid)
  status = nf90_put_var(ncid, varid , noah%model%evap            , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "hflx", varid)
  status = nf90_put_var(ncid, varid , noah%model%hflx            , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "ep", varid)
  status = nf90_put_var(ncid, varid , noah%model%ep              , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "runoff", varid)
  status = nf90_put_var(ncid, varid , noah%model%runoff          , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "cmm", varid)
  status = nf90_put_var(ncid, varid , noah%model%cmm             , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "chh", varid)
  status = nf90_put_var(ncid, varid , noah%model%chh             , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "evbs", varid)
  status = nf90_put_var(ncid, varid , noah%model%evbs            , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "evcw", varid)
  status = nf90_put_var(ncid, varid , noah%model%evcw            , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "sbsno", varid)
  status = nf90_put_var(ncid, varid , noah%model%sbsno           , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "snowc", varid)
  status = nf90_put_var(ncid, varid , noah%model%snowc           , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "stm", varid)
  status = nf90_put_var(ncid, varid , noah%model%stm             , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "snohf", varid)
  status = nf90_put_var(ncid, varid , noah%model%snohf           , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "smcwlt2", varid)
  status = nf90_put_var(ncid, varid , noah%model%smcwlt2         , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "smcref2", varid)
  status = nf90_put_var(ncid, varid , noah%model%smcref2         , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  status = nf90_inq_varid(ncid, "wet1", varid)
  status = nf90_put_var(ncid, varid , noah%model%wet1            , &
      start = (/namelist%subset_start,this%output_counter/), count = (/namelist%subset_length, 1/))

  
  status = nf90_close(ncid)

  this%output_counter = this%output_counter + 1
  
  end subroutine WriteOutputNoah
  
  subroutine WriteOutputNoahMP(this, namelist, noahmp, forcing, now_time)
  
  use mpi
  use netcdf
  use time_utilities
  use error_handling, only : handle_err
  use NamelistRead
  use ufsLandNoahMPType
  use ufsLandForcingModule
  use ufsLandGenericIO

  class(output_type)   :: this  
  type(namelist_type)  :: namelist
  type(noahmp_type)    :: noahmp
  type (forcing_type)  :: forcing
  double precision     :: now_time
  character*19     :: nowdate    ! current date
  integer          :: yyyy,mm,dd,hh,nn,ss
  integer :: ncid, dimid, varid, status
  integer :: dim_id_time, dim_id_loc, dim_id_soil, dim_id_snow, dim_id_snso, dim_id_date, dim_id_rad

  integer, parameter :: output = 1, restart = 2

  if(now_time == namelist%initial_time + namelist%timestep_seconds .or. &
     namelist%separate_output ) then
  
    call date_from_since(namelist%reference_date, now_time, nowdate)
    read(nowdate( 1: 4),'(i4.4)') yyyy
    read(nowdate( 6: 7),'(i2.2)') mm
    read(nowdate( 9:10),'(i2.2)') dd
    read(nowdate(12:13),'(i2.2)') hh
    read(nowdate(15:16),'(i2.2)') nn
    read(nowdate(18:19),'(i2.2)') ss

    write(this%filename,'(a16,i4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a3)') &
      "ufs_land_output.", yyyy, "-", mm, "-", dd, "_", hh, "-", nn, "-", ss, ".nc"

    this%filename = trim(namelist%output_dir)//"/"//trim(this%filename)

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

    status = nf90_def_var(ncid, "timestep", NF90_FLOAT, (/dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "time step")
      status = nf90_put_att(ncid, varid, "units", "seconds")

    call DefineNoahMP(output, noahmp, ncid, &
                      dim_id_time, dim_id_loc, dim_id_soil, &
                      dim_id_snow, dim_id_snso, dim_id_date, dim_id_rad)


    status = nf90_enddef(ncid)

    status = nf90_close(ncid)
    
    this%output_counter = 1

  end if
  
  status = nf90_open(this%filename, NF90_WRITE, ncid, comm = MPI_COMM_WORLD, &
       info = MPI_INFO_NULL)
   if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "time", varid)
  status = nf90_var_par_access(ncid, varid, NF90_COLLECTIVE)
  status = nf90_put_var(ncid, varid , now_time , start = (/this%output_counter/))
  
  status = nf90_inq_varid(ncid, "timestep", varid)
  status = nf90_var_par_access(ncid, varid, NF90_COLLECTIVE)
  status = nf90_put_var(ncid, varid , noahmp%static%timestep   )

  call WriteNoahMP(output, namelist, noahmp, ncid, this%output_counter)

  status = nf90_close(ncid)

  this%output_counter = this%output_counter + 1
  
  end subroutine WriteOutputNoahMP
  

end module ufsLandIOModule
