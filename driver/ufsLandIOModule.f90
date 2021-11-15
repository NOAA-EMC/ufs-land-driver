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
  character*19     :: reference_date = "1970-01-01 00:00:00"
  integer          :: yyyy,mm,dd,hh,nn,ss
  integer :: ncid, dimid, varid, status
  integer :: dim_id_time, dim_id_loc, dim_id_soil, dim_id_date
  integer :: outsub
  
  if(now_time == namelist%initial_time + namelist%timestep_seconds .or. &
     namelist%separate_output) then
  
    call date_from_since(reference_date, now_time, nowdate)
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

    status = nf90_create(this%filename, NF90_CLOBBER, ncid)
      if (status /= nf90_noerr) call handle_err(status)

! Define dimensions in the file.

    status = nf90_def_dim(ncid, "location"   , noah%static%im        , dim_id_loc)
      if (status /= nf90_noerr) call handle_err(status)
    status = nf90_def_dim(ncid, "soil_levels"   , noah%static%km     , dim_id_soil)
      if (status /= nf90_noerr) call handle_err(status)
    status = nf90_def_dim(ncid, "time"       , NF90_UNLIMITED        , dim_id_time)
      if (status /= nf90_noerr) call handle_err(status)
  
! Define variables in the file.

    status = nf90_def_var(ncid, "time", NF90_DOUBLE, dim_id_time, varid)
      status = nf90_put_att(ncid, varid, "long_name", "time")
      status = nf90_put_att(ncid, varid, "units", "seconds since "//reference_date)

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
  
  outsub = namelist%begsub - namelist%begloc + 1
  
  status = nf90_open(this%filename, NF90_WRITE, ncid)
   if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "time", varid)
  status = nf90_put_var(ncid, varid , now_time           , start = (/this%output_counter/))
  
  status = nf90_inq_varid(ncid, "delt", varid)
  status = nf90_put_var(ncid, varid , noah%static%delt   , start = (/this%output_counter/))

  status = nf90_inq_varid(ncid, "ps", varid)
  status = nf90_put_var(ncid, varid , forcing%surface_pressure   , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "t1", varid)
  status = nf90_put_var(ncid, varid , forcing%temperature        , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "q1", varid)
  status = nf90_put_var(ncid, varid , forcing%specific_humidity  , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "wind", varid)
  status = nf90_put_var(ncid, varid , forcing%wind_speed         , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "tprcp", varid)
  status = nf90_put_var(ncid, varid , forcing%precipitation      , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "dlwflx", varid)
  status = nf90_put_var(ncid, varid , forcing%downward_longwave  , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "dswsfc", varid)
  status = nf90_put_var(ncid, varid , forcing%downward_shortwave , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "sigmaf", varid)
  status = nf90_put_var(ncid, varid , noah%model%sigmaf          , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "sfcemis", varid)
  status = nf90_put_var(ncid, varid , noah%model%sfcemis         , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "snet", varid)
  status = nf90_put_var(ncid, varid , noah%model%snet            , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "tg3", varid)
  status = nf90_put_var(ncid, varid , noah%model%tg3             , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "cm", varid)
  status = nf90_put_var(ncid, varid , noah%model%cm              , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "ch", varid)
  status = nf90_put_var(ncid, varid , noah%model%ch              , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "prsl1", varid)
  status = nf90_put_var(ncid, varid , noah%model%prsl1           , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "prslki", varid)
  status = nf90_put_var(ncid, varid , noah%model%prslki          , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "zf", varid)
  status = nf90_put_var(ncid, varid , noah%model%zf              , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "shdmin", varid)
  status = nf90_put_var(ncid, varid , noah%model%shdmin          , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "shdmax", varid)
  status = nf90_put_var(ncid, varid , noah%model%shdmax          , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "snoalb", varid)
  status = nf90_put_var(ncid, varid , noah%model%snoalb          , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "sfalb", varid)
  status = nf90_put_var(ncid, varid , noah%model%sfalb           , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "weasd", varid)
  status = nf90_put_var(ncid, varid , noah%model%weasd           , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "snwdph", varid)
  status = nf90_put_var(ncid, varid , noah%model%snwdph          , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "tskin", varid)
  status = nf90_put_var(ncid, varid , noah%model%tskin           , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "srflag", varid)
  status = nf90_put_var(ncid, varid , noah%model%srflag          , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "smc", varid)
  status = nf90_put_var(ncid, varid , noah%model%smc                , &
      start = (/        outsub,              1,this%output_counter/), &
      count = (/noah%static%im, noah%static%km,                  1/))

  status = nf90_inq_varid(ncid, "stc", varid)
  status = nf90_put_var(ncid, varid , noah%model%stc                , &
      start = (/        outsub,              1,this%output_counter/), &
      count = (/noah%static%im, noah%static%km,                  1/))

  status = nf90_inq_varid(ncid, "slc", varid)
  status = nf90_put_var(ncid, varid , noah%model%slc                , &
      start = (/        outsub,              1,this%output_counter/), &
      count = (/noah%static%im, noah%static%km,                  1/))

  status = nf90_inq_varid(ncid, "canopy", varid)
  status = nf90_put_var(ncid, varid , noah%model%canopy          , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "trans", varid)
  status = nf90_put_var(ncid, varid , noah%model%trans           , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "tsurf", varid)
  status = nf90_put_var(ncid, varid , noah%model%tsurf           , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "zorl", varid)
  status = nf90_put_var(ncid, varid , noah%model%zorl            , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "sncovr1", varid)
  status = nf90_put_var(ncid, varid , noah%model%sncovr1         , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "qsurf", varid)
  status = nf90_put_var(ncid, varid , noah%model%qsurf           , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "gflux", varid)
  status = nf90_put_var(ncid, varid , noah%model%gflux           , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "drain", varid)
  status = nf90_put_var(ncid, varid , noah%model%drain           , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "evap", varid)
  status = nf90_put_var(ncid, varid , noah%model%evap            , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "hflx", varid)
  status = nf90_put_var(ncid, varid , noah%model%hflx            , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "ep", varid)
  status = nf90_put_var(ncid, varid , noah%model%ep              , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "runoff", varid)
  status = nf90_put_var(ncid, varid , noah%model%runoff          , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "cmm", varid)
  status = nf90_put_var(ncid, varid , noah%model%cmm             , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "chh", varid)
  status = nf90_put_var(ncid, varid , noah%model%chh             , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "evbs", varid)
  status = nf90_put_var(ncid, varid , noah%model%evbs            , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "evcw", varid)
  status = nf90_put_var(ncid, varid , noah%model%evcw            , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "sbsno", varid)
  status = nf90_put_var(ncid, varid , noah%model%sbsno           , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "snowc", varid)
  status = nf90_put_var(ncid, varid , noah%model%snowc           , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "stm", varid)
  status = nf90_put_var(ncid, varid , noah%model%stm             , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "snohf", varid)
  status = nf90_put_var(ncid, varid , noah%model%snohf           , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "smcwlt2", varid)
  status = nf90_put_var(ncid, varid , noah%model%smcwlt2         , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "smcref2", varid)
  status = nf90_put_var(ncid, varid , noah%model%smcref2         , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  status = nf90_inq_varid(ncid, "wet1", varid)
  status = nf90_put_var(ncid, varid , noah%model%wet1            , &
      start = (/outsub,this%output_counter/), count = (/noah%static%im, 1/))

  
  status = nf90_close(ncid)

  this%output_counter = this%output_counter + 1
  
  end subroutine WriteOutputNoah
  
  subroutine WriteOutputNoahMP(this, namelist, noahmp, forcing, now_time)
  
  use netcdf
  use time_utilities
  use error_handling, only : handle_err
  use NamelistRead
  use ufsLandNoahMPType
  use ufsLandForcingModule

  class(output_type)   :: this  
  type(namelist_type)  :: namelist
  type(noahmp_type)    :: noahmp
  type (forcing_type)  :: forcing
  double precision     :: now_time
  character*19     :: nowdate    ! current date
  character*19     :: reference_date = "1970-01-01 00:00:00"
  integer          :: yyyy,mm,dd,hh,nn,ss
  integer :: ncid, dimid, varid, status
  integer :: dim_id_time, dim_id_loc, dim_id_soil, dim_id_snow, dim_id_snso, dim_id_date
  integer :: outsub
  
  if(now_time == namelist%initial_time + namelist%timestep_seconds .or. &
     namelist%separate_output ) then
  
    call date_from_since(reference_date, now_time, nowdate)
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

    status = nf90_create(this%filename, NF90_CLOBBER, ncid)
      if (status /= nf90_noerr) call handle_err(status)

! Define dimensions in the file.

    status = nf90_def_dim(ncid, "location"    , noahmp%static%vector_length   , dim_id_loc)
      if (status /= nf90_noerr) call handle_err(status)
    status = nf90_def_dim(ncid, "soil_levels" , noahmp%static%soil_levels     , dim_id_soil)
      if (status /= nf90_noerr) call handle_err(status)
    status = nf90_def_dim(ncid, "snow_levels" , 3                             , dim_id_snow)
      if (status /= nf90_noerr) call handle_err(status)
    status = nf90_def_dim(ncid, "snso_levels" , noahmp%static%soil_levels + 3 , dim_id_snso)
      if (status /= nf90_noerr) call handle_err(status)
    status = nf90_def_dim(ncid, "time"        , NF90_UNLIMITED                , dim_id_time)
      if (status /= nf90_noerr) call handle_err(status)
  
! Define variables in the file.

    status = nf90_def_var(ncid, "time", NF90_DOUBLE, dim_id_time, varid)
      status = nf90_put_att(ncid, varid, "long_name", "time")
      status = nf90_put_att(ncid, varid, "units", "seconds since "//reference_date)

    status = nf90_def_var(ncid, "timestep", NF90_FLOAT, (/dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "time step")
      status = nf90_put_att(ncid, varid, "units", "seconds")

    status = nf90_def_var(ncid, "pressure_surface", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface pressure")
      status = nf90_put_att(ncid, varid, "units", "Pa")

    status = nf90_def_var(ncid, "temperature_forcing", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "forcing-level temperature")
      status = nf90_put_att(ncid, varid, "units", "K")

    status = nf90_def_var(ncid, "spec_humidity_forcing", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "forcing-level specific humidity")
      status = nf90_put_att(ncid, varid, "units", "kg/kg")

    status = nf90_def_var(ncid, "vegetation_fraction", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "vegetation fraction")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_def_var(ncid, "emissivity_total", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface emissivity")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_def_var(ncid, "albedo_direct_vis", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface albedo - direct visible")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_def_var(ncid, "albedo_direct_nir", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface albedo - direct NIR")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_def_var(ncid, "albedo_diffuse_vis", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface albedo - diffuse visible")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_def_var(ncid, "albedo_diffuse_nir", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface albedo - diffuse NIR")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_def_var(ncid, "radiation_longwave_down", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "forcing longwave downward flux")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "radiation_shortwave_down", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "forcing shortwave downward flux")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "temperature_soil_bot", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "deep soil temperature")
      status = nf90_put_att(ncid, varid, "units", "K")

    status = nf90_def_var(ncid, "cm_noahmp", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface exchange coeff for momentum")
      status = nf90_put_att(ncid, varid, "units", "m/s")

    status = nf90_def_var(ncid, "ch_noahmp", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface exchange coeff heat & moisture")
      status = nf90_put_att(ncid, varid, "units", "m/s")

    status = nf90_def_var(ncid, "forcing_height", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "height of forcing")
      status = nf90_put_att(ncid, varid, "units", "m")

    status = nf90_def_var(ncid, "wind_speed", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "wind speed")
      status = nf90_put_att(ncid, varid, "units", "m/s")
 
    status = nf90_def_var(ncid, "max_vegetation_frac", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "maximum fractional coverage of vegetation")
      status = nf90_put_att(ncid, varid, "units", "fraction")

    status = nf90_def_var(ncid, "albedo_total", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "grid composity albedo")
      status = nf90_put_att(ncid, varid, "units", "fraction")

    status = nf90_def_var(ncid, "snow_water_equiv", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "snow water equivalent")
      status = nf90_put_att(ncid, varid, "units", "mm")

    status = nf90_def_var(ncid, "snow_depth", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "snow depth")
      status = nf90_put_att(ncid, varid, "units", "m")

    status = nf90_def_var(ncid, "temperature_radiative", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface radiative temperature")
      status = nf90_put_att(ncid, varid, "units", "K")

    status = nf90_def_var(ncid, "precipitation_forcing", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "total precipitation during time integration")
      status = nf90_put_att(ncid, varid, "units", "mm")

    status = nf90_def_var(ncid, "soil_moisture_vol", NF90_FLOAT, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "volumetric moisture content in soil level")
      status = nf90_put_att(ncid, varid, "units", "m3/m3")

    status = nf90_def_var(ncid, "temperature_soil", NF90_FLOAT, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "temperature in soil level")
      status = nf90_put_att(ncid, varid, "units", "K")

    status = nf90_def_var(ncid, "soil_liquid_vol", NF90_FLOAT, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "volumetric liquid content in soil level")
      status = nf90_put_att(ncid, varid, "units", "m3/m3")

    status = nf90_def_var(ncid, "canopy_water", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "canopy moisture content")
      status = nf90_put_att(ncid, varid, "units", "m")

    status = nf90_def_var(ncid, "transpiration_heat", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "plant transpiration")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "z0_total", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface roughness")
      status = nf90_put_att(ncid, varid, "units", "m")

    status = nf90_def_var(ncid, "snow_cover_fraction", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "snow cover fraction")
      status = nf90_put_att(ncid, varid, "units", "fraction")

    status = nf90_def_var(ncid, "spec_humidity_surface", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "diagnostic specific humidity at sfc")
      status = nf90_put_att(ncid, varid, "units", "kg/kg")

    status = nf90_def_var(ncid, "ground_heat_total", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "soil heat flux")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "runoff_baseflow", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "drainage runoff")
      status = nf90_put_att(ncid, varid, "units", "mm/s")

    status = nf90_def_var(ncid, "latent_heat_total", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "latent heat flux")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "sensible_heat_total", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "sensible heat flux")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "evaporation_potential", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "potential evaporation")
      status = nf90_put_att(ncid, varid, "units", "mm/s")

    status = nf90_def_var(ncid, "runoff_surface", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "surface runoff")
      status = nf90_put_att(ncid, varid, "units", "mm/s")

    status = nf90_def_var(ncid, "latent_heat_ground", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "direct soil latent heat flux")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "latent_heat_canopy", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "canopy water latent heat flux")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "snow_sublimation", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "sublimation/deposit from snowpack")
      status = nf90_put_att(ncid, varid, "units", "mm/s")

    status = nf90_def_var(ncid, "soil_moisture_total", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "total soil column moisture content")
      status = nf90_put_att(ncid, varid, "units", "mm")

    status = nf90_def_var(ncid, "precip_adv_heat_total", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "precipitation advected heat - total")
      status = nf90_put_att(ncid, varid, "units", "W/m2")

    status = nf90_def_var(ncid, "cosine_zenith", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "cosine of zenith angle")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_def_var(ncid, "snow_levels", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "active snow levels")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_def_var(ncid, "temperature_leaf", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "leaf temperature")
      status = nf90_put_att(ncid, varid, "units", "K")

    status = nf90_def_var(ncid, "temperature_ground", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "ground temperature")
      status = nf90_put_att(ncid, varid, "units", "K")

    status = nf90_def_var(ncid, "canopy_ice", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "canopy ice")
      status = nf90_put_att(ncid, varid, "units", "mm")

    status = nf90_def_var(ncid, "canopy_liquid", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "canopy liquid")
      status = nf90_put_att(ncid, varid, "units", "mm")

    status = nf90_def_var(ncid, "vapor_pres_canopy_air", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "")
      status = nf90_put_att(ncid, varid, "units", "")

    status = nf90_def_var(ncid, "temperature_canopy_air", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "")
      status = nf90_put_att(ncid, varid, "units", "")

    status = nf90_def_var(ncid, "canopy_wet_fraction", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "fraction of canopy covered by water")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_def_var(ncid, "snow_water_equiv_old", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "snow water equivalent - before integration")
      status = nf90_put_att(ncid, varid, "units", "mm")

    status = nf90_def_var(ncid, "snow_albedo_old", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "snow albedo - before integration")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_def_var(ncid, "snowfall", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "snowfall")
      status = nf90_put_att(ncid, varid, "units", "mm/s")

    status = nf90_def_var(ncid, "lake_water", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "")
      status = nf90_put_att(ncid, varid, "units", "")

    status = nf90_def_var(ncid, "depth_water_table", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "depth to water table")
      status = nf90_put_att(ncid, varid, "units", "m")

    status = nf90_def_var(ncid, "aquifer_water", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "aquifer water content")
      status = nf90_put_att(ncid, varid, "units", "mm")

    status = nf90_def_var(ncid, "saturated_water", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "aquifer + saturated soil water content")
      status = nf90_put_att(ncid, varid, "units", "mm")

    status = nf90_def_var(ncid, "leaf_carbon", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "carbon in leaves")
      status = nf90_put_att(ncid, varid, "units", "g/m2")

    status = nf90_def_var(ncid, "root_carbon", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "carbon in roots")
      status = nf90_put_att(ncid, varid, "units", "g/m2")

    status = nf90_def_var(ncid, "stem_carbon", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "carbon in stems")
      status = nf90_put_att(ncid, varid, "units", "g/m2")

    status = nf90_def_var(ncid, "wood_carbon", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "carbon in wood")
      status = nf90_put_att(ncid, varid, "units", "g/m2")

    status = nf90_def_var(ncid, "soil_carbon_stable", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "stable carbon in soil")
      status = nf90_put_att(ncid, varid, "units", "g/m2")

    status = nf90_def_var(ncid, "soil_carbon_fast", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "fast carbon in soil")
      status = nf90_put_att(ncid, varid, "units", "g/m2")

    status = nf90_def_var(ncid, "leaf_area_index", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "leaf area index")
      status = nf90_put_att(ncid, varid, "units", "m2/m2")

    status = nf90_def_var(ncid, "stem_area_index", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "stem area index")
      status = nf90_put_att(ncid, varid, "units", "m2/m2")

    status = nf90_def_var(ncid, "snow_age", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "BATS non-dimensional snow age")
      status = nf90_put_att(ncid, varid, "units", "-")

    status = nf90_def_var(ncid, "soil_moisture_wtd", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "soil water content between bottom of the soil and water table")
      status = nf90_put_att(ncid, varid, "units", "m3/m3")

    status = nf90_def_var(ncid, "deep_recharge", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "deep recharge for runoff_option 5")
      status = nf90_put_att(ncid, varid, "units", "m")

    status = nf90_def_var(ncid, "recharge", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "recharge for runoff_option 5")
      status = nf90_put_att(ncid, varid, "units", "m")

    status = nf90_def_var(ncid, "temperature_2m", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "grid diagnostic temperature at 2 meters")
      status = nf90_put_att(ncid, varid, "units", "K")

    status = nf90_def_var(ncid, "spec_humidity_2m", NF90_FLOAT, (/dim_id_loc,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "grid diagnostic specific humidity at 2 meters")
      status = nf90_put_att(ncid, varid, "units", "kg/kg")

    status = nf90_def_var(ncid, "eq_soil_water_vol", NF90_FLOAT, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "equilibrium soil water content")
      status = nf90_put_att(ncid, varid, "units", "m3/m3")

    status = nf90_def_var(ncid, "interface_depth", NF90_FLOAT, (/dim_id_loc,dim_id_snso,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "layer-bottom depth from snow surface")
      status = nf90_put_att(ncid, varid, "units", "m")

    status = nf90_def_var(ncid, "temperature_snow", NF90_FLOAT, (/dim_id_loc,dim_id_snow,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "snow level temperature")
      status = nf90_put_att(ncid, varid, "units", "K")

    status = nf90_def_var(ncid, "snow_level_ice", NF90_FLOAT, (/dim_id_loc,dim_id_snow,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "ice content of snow levels")
      status = nf90_put_att(ncid, varid, "units", "mm")

    status = nf90_def_var(ncid, "snow_level_liquid", NF90_FLOAT, (/dim_id_loc,dim_id_snow,dim_id_time/), varid)
      status = nf90_put_att(ncid, varid, "long_name", "liquid content of snow levels")
      status = nf90_put_att(ncid, varid, "units", "mm")

    status = nf90_enddef(ncid)

    status = nf90_close(ncid)
    
    this%output_counter = 1

  end if
  
  outsub = namelist%begsub - namelist%begloc + 1
  
  status = nf90_open(this%filename, NF90_WRITE, ncid)
   if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "time", varid)
  status = nf90_put_var(ncid, varid , now_time             , start = (/this%output_counter/))
  
  status = nf90_inq_varid(ncid, "timestep", varid)
  status = nf90_put_var(ncid, varid , noahmp%static%timestep   )

  status = nf90_inq_varid(ncid, "pressure_surface", varid)
  status = nf90_put_var(ncid, varid , forcing%surface_pressure   , &
      start = (/outsub,this%output_counter/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_forcing", varid)
  status = nf90_put_var(ncid, varid , forcing%temperature        , &
      start = (/outsub,this%output_counter/), count = (/noahmp%static%vector_length, 1/))
      
  status = nf90_inq_varid(ncid, "spec_humidity_forcing", varid)
  status = nf90_put_var(ncid, varid , forcing%specific_humidity  , &
      start = (/outsub,this%output_counter/), count = (/noahmp%static%vector_length, 1/))
      
  status = nf90_inq_varid(ncid, "wind_speed", varid)
  status = nf90_put_var(ncid, varid , forcing%wind_speed         , &
      start = (/outsub,this%output_counter/), count = (/noahmp%static%vector_length, 1/))
      
  status = nf90_inq_varid(ncid, "precipitation_forcing", varid)
  status = nf90_put_var(ncid, varid , forcing%precipitation      , &
      start = (/outsub,this%output_counter/), count = (/noahmp%static%vector_length, 1/))
      
  status = nf90_inq_varid(ncid, "radiation_longwave_down", varid)
  status = nf90_put_var(ncid, varid , forcing%downward_longwave  , &
      start = (/outsub,this%output_counter/), count = (/noahmp%static%vector_length, 1/))
      
  status = nf90_inq_varid(ncid, "radiation_shortwave_down", varid)
  status = nf90_put_var(ncid, varid , forcing%downward_shortwave , &
      start = (/outsub,this%output_counter/), count = (/noahmp%static%vector_length, 1/))
      
  status = nf90_inq_varid(ncid, "vegetation_fraction", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%vegetation_fraction     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "emissivity_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%emissivity_total        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_direct_vis", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%albedo_direct(:,1)      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_direct_nir", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%albedo_direct(:,2)      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_diffuse_vis", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%albedo_diffuse(:,1)     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_diffuse_nir", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%albedo_diffuse(:,2)     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_soil_bot", varid)
  status = nf90_put_var(ncid, varid , noahmp%static%temperature_soil_bot  , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "cm_noahmp", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%cm_noahmp              , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "ch_noahmp", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%ch_noahmp              , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "forcing_height", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%forcing_height         , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "max_vegetation_frac", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%max_vegetation_frac    , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "albedo_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%albedo_total            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_water_equiv", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%snow_water_equiv       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_depth", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%snow_depth             , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_radiative", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%temperature_radiative  , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_moisture_vol", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%soil_moisture_vol        , &
      start = (/outsub                     ,                         1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels, 1/))

  status = nf90_inq_varid(ncid, "temperature_soil", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%temperature_soil         , &
      start = (/outsub                     ,                         1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels, 1/))

  status = nf90_inq_varid(ncid, "soil_liquid_vol", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%soil_liquid_vol          , &
      start = (/outsub                     ,                         1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels, 1/))

  status = nf90_inq_varid(ncid, "canopy_water", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%canopy_water            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "transpiration_heat", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%transpiration_heat      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "z0_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%z0_total                , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_cover_fraction", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%snow_cover_fraction     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "spec_humidity_surface", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%spec_humidity_surface   , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "ground_heat_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%ground_heat_total       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "runoff_baseflow", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%runoff_baseflow         , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "latent_heat_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%latent_heat_total       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "sensible_heat_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%sensible_heat_total     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "evaporation_potential", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%evaporation_potential   , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "runoff_surface", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%runoff_surface          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "latent_heat_ground", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%latent_heat_ground      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "latent_heat_canopy", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%latent_heat_canopy      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_sublimation", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%snow_sublimation        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_moisture_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%soil_moisture_total     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "precip_adv_heat_total", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%precip_adv_heat_total   , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "cosine_zenith", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%cosine_zenith          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_levels", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%snow_levels            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_leaf", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%temperature_leaf       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_ground", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%temperature_ground     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "canopy_ice", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%canopy_ice             , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "canopy_liquid", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%canopy_liquid          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "vapor_pres_canopy_air", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%vapor_pres_canopy_air  , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_canopy_air", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%temperature_canopy_air , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "canopy_wet_fraction", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%canopy_wet_fraction     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_water_equiv_old", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%snow_water_equiv_old   , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_albedo_old", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%snow_albedo_old         , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snowfall", varid)
  status = nf90_put_var(ncid, varid , noahmp%forcing%snowfall             , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "lake_water", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%lake_water             , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "depth_water_table", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%depth_water_table       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "aquifer_water", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%aquifer_water          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "saturated_water", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%saturated_water        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "leaf_carbon", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%leaf_carbon            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "root_carbon", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%root_carbon            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "stem_carbon", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%stem_carbon            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "wood_carbon", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%wood_carbon            , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_carbon_stable", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%soil_carbon_stable     , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_carbon_fast", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%soil_carbon_fast       , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "leaf_area_index", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%leaf_area_index         , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "stem_area_index", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%stem_area_index         , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "snow_age", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%snow_age               , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "soil_moisture_wtd", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%soil_moisture_wtd      , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "deep_recharge", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%deep_recharge           , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "recharge", varid)
  status = nf90_put_var(ncid, varid , noahmp%flux%recharge                , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "temperature_2m", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%temperature_2m          , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "spec_humidity_2m", varid)
  status = nf90_put_var(ncid, varid , noahmp%diag%spec_humidity_2m        , &
      start = (/outsub,1/), count = (/noahmp%static%vector_length, 1/))

  status = nf90_inq_varid(ncid, "eq_soil_water_vol", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%eq_soil_water_vol          , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels  , 1/))

  status = nf90_inq_varid(ncid, "interface_depth", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%interface_depth            , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length, noahmp%static%soil_levels+3, 1/))

  status = nf90_inq_varid(ncid, "temperature_snow", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%temperature_snow           , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length,                           3, 1/))

  status = nf90_inq_varid(ncid, "snow_level_ice", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%snow_level_ice             , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length,                           3, 1/))

  status = nf90_inq_varid(ncid, "snow_level_liquid", varid)
  status = nf90_put_var(ncid, varid , noahmp%state%snow_level_liquid          , &
      start = (/outsub                     ,                           1, 1/) , &
      count = (/noahmp%static%vector_length,                           3, 1/))
  
  status = nf90_close(ncid)

  this%output_counter = this%output_counter + 1
  
  end subroutine WriteOutputNoahMP
  

end module ufsLandIOModule
